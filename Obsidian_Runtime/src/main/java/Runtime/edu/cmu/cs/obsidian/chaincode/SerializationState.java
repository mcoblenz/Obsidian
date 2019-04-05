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



public class SerializationState {
    private Map<String, ObsidianSerialized> guidMap;

    /* When we return an object to the client, we have to be able to look up that object later by its GUID.
     * On lookup, we need to be able to create a lazily-initialized object, which requires that we know the name of its class.
     *
     * */
    private HashMap<String, ReturnedReferenceState> returnedObjectClassMapAtBeginningOfTransaction;
    private HashMap<String, ReturnedReferenceState> returnedObjectClassMap;


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
        guidMap.put(guid, obj);
    }

    public void flushEntries() {
        // Fabric requires that all endorsers produce identical read sets.
        // But the peer on which instantiation happened will have some objects cached, resulting in an
        // inconsistent write set with the other peers.
        // To work around this problem, we flush all flushable objects.

        for (ObsidianSerialized obj : guidMap.values()) {
            obj.flush();
        }
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
        if (returnedObjectClassMap == null) {
            returnedObjectClassMap = new HashMap<String, ReturnedReferenceState>();
        }
        // TODO: put this back
        //  loadReturnedObjectsMap(stub);

        System.out.println("mapReturnedObject: " + obj.__getGUID() + ". new external ownership status: " + returnedReferenceIsOwned);
        returnedObjectClassMap.put(obj.__getGUID(), new ReturnedReferenceState(obj.getClass(), returnedReferenceIsOwned));
    }

    public ReturnedReferenceState getReturnedReferenceState(ChaincodeStub stub, String guid) {
        loadReturnedObjectsMap(stub);
        for (Map.Entry<String, ReturnedReferenceState> item : returnedObjectClassMap.entrySet()) {
            String key = item.getKey();
            if (key.equals(guid)) {
                System.out.println("Are equal");
            } else {
                System.out.println("Are not equal");
            }
            ReturnedReferenceState value = item.getValue();
            System.out.println("Key: " + key + ", Value: " + value);
        }
        System.out.println("GUID: " + guid);
        System.out.println("Return from map: " + returnedObjectClassMap.get(guid));
        return returnedObjectClassMap.get(guid);
    }

    public void archiveReturnedObjectsMap (ChaincodeStub stub) {
        System.out.println("archiveReturnedObjectsMap 2");


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
                int numReturnedObjects = Integer.parseInt(numAsString);

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

                    System.out.println("loaded returned object " + guid + ": " + c + "; is owned: " + isOwnedStr);

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
/*
        public void archiveReturnedObjectsMap (ChaincodeStub stub) {
        // TODO: remove old, stale entries?
        System.out.println("archiveReturnedObjectsMap");

        if (returnedObjectClassMap != null) {
            for (Map.Entry<String, ReturnedReferenceState> entry : returnedObjectClassMap.entrySet()) {
                CompositeKey classKey = stub.createCompositeKey(s_returnedObjectsClassMapKey, entry.getKey());
                // the class key is s_returnedObjectsClassMapKey + the ID of the object.
                // Store the key -> canonical name of the class.
                stub.putStringState(classKey.toString(), entry.getValue().getClassRef().getCanonicalName());

                // The owned key is s_returnedObjectsIsOwnedMapKey + the ID of the object.
                CompositeKey isOwnedKey = stub.createCompositeKey(s_returnedObjectsIsOwnedMapKey, entry.getKey());
                boolean isOwned = entry.getValue().getIsOwnedReference();
                byte[] isOwnedByteArray = isOwned ? TRUE_BYTE : FALSE_BYTE;
                stub.putState(isOwnedKey.toString(), isOwnedByteArray);
                System.out.println("archiving returned object: " + classKey + "->" + entry.getValue().getClassRef().getCanonicalName());
            }
        }
    }

    private void loadReturnedObjectsMap (ChaincodeStub stub) {
        if (returnedObjectClassMap == null) {
            returnedObjectClassMap = new HashMap<String, ReturnedReferenceState>();

            QueryResultsIterator<KeyValue> results = stub.getStateByPartialCompositeKey(s_returnedObjectsClassMapKey);

            for (KeyValue kv : results) {
                try {
                    System.out.println("key to load: " + kv.getKey());
                    System.out.println("class name: " + kv.getStringValue());
                    Class c = Class.forName(kv.getStringValue());
                    CompositeKey isOwnedKey = stub.createCompositeKey(s_returnedObjectsIsOwnedMapKey, kv.getKey().replace("\0", ""));
                    byte[] isOwnedByteArray = stub.getState(isOwnedKey.toString());
                    boolean isOwned = (isOwnedByteArray == TRUE_BYTE) ? true : false;

                    returnedObjectClassMap.put(kv.getKey().replace(s_returnedObjectsClassMapKey, "").replace("\0", ""), new ReturnedReferenceState(c, isOwned));
                    System.out.println("loading map: " + kv.getKey() + " -> " + c);
                }
                catch (ClassNotFoundException e) {
                    System.err.println("Failed to find a Class object for class name " + kv.getValue());
                }
            }
        }
    }
    */

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

            if (receiverContract == null) {
                try {
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
        }

        return receiverContract;
    }
}
