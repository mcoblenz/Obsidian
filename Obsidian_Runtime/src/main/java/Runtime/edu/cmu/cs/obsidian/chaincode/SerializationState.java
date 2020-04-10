/**
 * Created by Miles Baker, 7/25/18.
 *
 * Holds the current serialization state of a contract.
 * It contains a map of GUIDs to already-deserialized
 * objects, and a chaincode stub to get objects out
 * of the blockchain that aren't already loaded. */
package edu.cmu.cs.obsidian.chaincode;

import org.hyperledger.fabric.shim.ChaincodeStub;
import org.hyperledger.fabric.shim.ledger.*;

import edu.cmu.cs.obsidian.chaincode.ObsidianSerialized;
import java.util.Map;
import java.util.HashMap;
import java.util.HashSet;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.logging.*;

public class SerializationState {
    private Logger logger;
    private Map<String, ObsidianSerialized> guidMap;

    /* When we return an object to the client, we have to be able to look up that object later by its GUID.
     * On lookup, we need to be able to create a lazily-initialized object, which requires that we know the name of its class.
     *
     * TODO: Only store whether the object is an owned reference, since allObjectClasses stores class names for all objects.
     * */
    private HashMap<String, ReturnedReferenceState> returnedObjectClassMapAtBeginningOfTransaction;
    private HashMap<String, ReturnedReferenceState> returnedObjectClassMap;

    // When we unarchive an object that has a field that references another object,
    // if the referenced object is a more specific type than the field type,
    // we can't tell what object to instantiate.
    // To solve this problem, we record type information for all objects in the heap.
    // This map is from GUID to Java class.
    // Some of this information is redundant with information in guidMap, but guidMap is not guaranteed to be populated for all reachable objects.
    private HashMap<String, Class> allObjectClasses;

    private HashSet<Object> stateLockedObjects;

    static final String s_returnedObjectsClassMapKey = "ReturnedObject";
    static final String s_returnedObjectsIsOwnedMapKey = "ReturnedObjectIsOwned";

    final byte[] FALSE_BYTE = {0};
    final byte[] TRUE_BYTE = {1};

    private ChaincodeStub stub;
    UUIDFactory uuidFactory;

    public SerializationState() {
        guidMap = new HashMap<String, ObsidianSerialized>();
        stateLockedObjects = new HashSet<Object>();
        allObjectClasses = new HashMap<String, Class>();
        logger = Logger.getLogger("SerializationStateLogger");
    }

    public void setStub(ChaincodeStub newStub) {
        stub = newStub;
        uuidFactory = new UUIDFactory(newStub.getTxId());
    }

    public ChaincodeStub getStub() {
        return stub;
    }

    public ObsidianSerialized getEntry(String guid) {
        return guidMap.get(guid);
    }

    public void putEntry(String guid, ObsidianSerialized obj) {
        Exception e = new Exception();
        System.out.println("putEntry stack trace: ");
        e.printStackTrace();
        guidMap.put(guid, obj);
        allObjectClasses.put(guid, obj.getClass());
    }

    public void flushEntries() {
        // Fabric requires that all endorsers produce identical read sets.
        // But the peer on which instantiation happened will have some objects cached, resulting in an
        // inconsistent write set with the other peers.
        // To work around this problem, we flush all flushable objects.

        for (ObsidianSerialized obj : guidMap.values()) {
            obj.flush();
        }

        returnedObjectClassMap = null;
        allObjectClasses = null;
    }

    // TODO: move this to another class, since it pertains to clients too (and does not pertain to serialization).
    public void beginStateLock(Object obj) {
        stateLockedObjects.add(obj);
    }

    public void endStateLock(Object obj) {
        stateLockedObjects.remove(obj);
    }

    public boolean objectIsStateLocked(Object obj) {
        return stateLockedObjects.contains(obj);
    }

    public void beginTransaction() {
        // Shallow clone suffices because ReturnedReferenceState is immutable.
        if (returnedObjectClassMap != null) {
            returnedObjectClassMapAtBeginningOfTransaction = (HashMap<String, ReturnedReferenceState>) returnedObjectClassMap.clone();
        }
    }

    public void transactionSucceeded() {
        returnedObjectClassMapAtBeginningOfTransaction = null;
    }

    public void transactionFailed() {
        returnedObjectClassMap = returnedObjectClassMapAtBeginningOfTransaction;
        returnedObjectClassMapAtBeginningOfTransaction = null;
        guidMap.clear();
    }

    // If the returned reference is owned, record that so that we can re-claim ownership when we see the object again.
    public void mapReturnedObject(ObsidianSerialized obj, boolean returnedReferenceIsOwned) {
        loadReturnedObjectsMap(stub);
        returnedObjectClassMap.put(obj.__getGUID(), new ReturnedReferenceState(obj.getClass(), returnedReferenceIsOwned));
    }

    public ReturnedReferenceState getReturnedReferenceState(ChaincodeStub stub, String guid) {
        loadReturnedObjectsMap(stub);
        return returnedObjectClassMap.get(guid);
    }

    public Class getClassOfObject(ChaincodeStub stub, String guid) {
        return allObjectClasses.get(guid);
    }

    public void archiveReturnedObjectsMap (ChaincodeStub stub) {
        if (returnedObjectClassMap != null) {
            // Archive the number of returned objects.
            stub.putStringState("NumReturnedObjects", Integer.toString(returnedObjectClassMap.size()));

            int i = 0;
            for (Map.Entry<String, ReturnedReferenceState> entry : returnedObjectClassMap.entrySet()) {
                String objGuid = entry.getKey();
                String guidKey = "ReturnedObjectGUID" + Integer.toString(i);
                stub.putStringState(guidKey, objGuid);

                String classNameKey = "ReturnedObjectClass" + Integer.toString(i);
                stub.putStringState(classNameKey, entry.getValue().getClassRef().getCanonicalName());

                String isOwnedKey = "ReturnedObjectIsOwned" + Integer.toString(i);
                boolean isOwned = entry.getValue().getIsOwnedReference();
                String isOwnedValue = isOwned ? "true" : "false";
                stub.putStringState(isOwnedKey, isOwnedValue);

                i++;
            }
        }
    }


    private void loadReturnedObjectsMap (ChaincodeStub stub){
        if (returnedObjectClassMap == null) {
            returnedObjectClassMap = new HashMap<String, ReturnedReferenceState>();

            try {
                String numAsString =  stub.getStringState("NumReturnedObjects");
                int numReturnedObjects = 0;

                if (numAsString != null && numAsString.length() > 0) {
                    numReturnedObjects = Integer.parseInt(numAsString);
                }

                for (int i = 0; i < numReturnedObjects; i++) {
                    String indexAsString = Integer.toString(i);

                    String guidKey = "ReturnedObjectGUID" + indexAsString;
                    String classNameKey = "ReturnedObjectClass" + indexAsString;
                    String isOwnedKey = "ReturnedObjectIsOwned" + indexAsString;

                    String guid = stub.getStringState(guidKey);
                    String objectClass = stub.getStringState(classNameKey);
                    Class c = Class.forName(objectClass);

                    String isOwnedStr = stub.getStringState(isOwnedKey);
                    boolean isOwned = isOwnedStr.equals("true");

                    ReturnedReferenceState refState = new ReturnedReferenceState(c, isOwned);
                    returnedObjectClassMap.put(guid, refState);
                }
            }
            catch (NumberFormatException e) {
                System.err.println("Failed to parse the number of returned objects: " + e);
            }
            catch (ClassNotFoundException e) {
                System.err.println("Failed to find a Class object for class name: " + e);
            }

        }
    }

    public void archiveObjectClasses(ChaincodeStub stub) {
        if (allObjectClasses != null) {
            // Archive the number of returned objects.
            stub.putStringState("NumObjectClasses", Integer.toString(allObjectClasses.size()));

            int i = 0;
            for (Map.Entry<String, Class> entry : allObjectClasses.entrySet()) {
                String objGuid = entry.getKey();
                String guidKey = "ObjectClassGUID" + Integer.toString(i);
                stub.putStringState(guidKey, objGuid);

                String classNameKey = "ObjectClass" + Integer.toString(i);
                stub.putStringState(classNameKey, entry.getValue().getCanonicalName());

                i++;
            }
        }
    }


    public void loadObjectClasses (ChaincodeStub stub) {
        // Load unconditionally so all peers load the same.
        allObjectClasses = new HashMap<String, Class>();

        try {
            String numAsString =  stub.getStringState("NumObjectClasses");
            int numObjects = 0;

            if (numAsString != null && numAsString.length() > 0) {
                numObjects = Integer.parseInt(numAsString);
            }

            for (int i = 0; i < numObjects; i++) {
                String indexAsString = Integer.toString(i);

                String guidKey = "ObjectClassGUID" + indexAsString;
                String classNameKey = "ObjectClass" + indexAsString;

                String guid = stub.getStringState(guidKey);
                String objectClass = stub.getStringState(classNameKey);
                Class c = Class.forName(objectClass);

                allObjectClasses.put(guid, c);
            }
        }
        catch (NumberFormatException e) {
            System.err.println("Failed to parse the number of returned objects: " + e);
        }
        catch (ClassNotFoundException e) {
            System.err.println("Failed to find a Class object for class name: " + e);
        }
    }

    public UUIDFactory getUUIDFactory() {
        return uuidFactory;
    }

    public void setUUIDFactory(UUIDFactory factory) { uuidFactory = factory; }


    // If we are loading an object and plan to take ownership, ensure this is allowed (and record that ownership has been taken).
    public ObsidianSerialized loadContractWithGUID(ChaincodeStub stub, String guid, boolean requireOwnership, boolean ownershipRemains) throws BadArgumentException, IllegalOwnershipConsumptionException {
        ObsidianSerialized receiverContract = getEntry(guid);

        ReturnedReferenceState refState = getReturnedReferenceState(stub, guid);
        if (refState == null) {
            System.err.println("Unable to find class of object to look up: " + guid);
            return null;
        } else {
            boolean outstandingOwnedReference = refState.getIsOwnedReference();

            if (receiverContract == null) {
                try {
                    // We haven't loaded this object yet. Make a shell object by calling its constructor; we'll load the fields lazily later.
                    Class objectClass = refState.getClassRef();
                    Constructor constructor = objectClass.getConstructor(String.class);
                    Object loadedObject = constructor.newInstance(guid);
                    if ((loadedObject instanceof ObsidianSerialized)) {
                        receiverContract = (ObsidianSerialized) loadedObject;
                        if (getEntry(guid) == null) {
                            putEntry(guid, receiverContract);
                        }

                    } else {
                        System.err.println("Loaded object is not an Obsidian object.");
                        throw new BadArgumentException(guid);
                    }
                } catch (NoSuchMethodException |
                        InstantiationException |
                        IllegalAccessException |
                        InvocationTargetException e) {
                    System.err.println("Caught exception constructing object to load: " + e);
                    return null;
                }
            }

            if (requireOwnership) {
                if (!outstandingOwnedReference) {
                    throw new IllegalOwnershipConsumptionException(guid);
                }
                else if (!ownershipRemains) {
                    // We've now consumed ownership, so record that fact.
                    // We can't remove the object from the map because the client may well still have a reference.
                    mapReturnedObject(receiverContract, false);
                }
            }
            else if (ownershipRemains && !outstandingOwnedReference) {
                mapReturnedObject(receiverContract, true);
            }

        }


        return receiverContract;
    }
}
