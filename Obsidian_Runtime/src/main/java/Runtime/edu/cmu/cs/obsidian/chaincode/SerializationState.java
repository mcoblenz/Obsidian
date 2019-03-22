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
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;



public class SerializationState {
    private Map<String, ObsidianSerialized> guidMap;

    /* When we return an object to the client, we have to be able to look up that object later by its GUID.
     * On lookup, we need to be able to create a lazily-initialized object, which requires that we know the name of its class.
     *
     * */
    private Map<String, ReturnedReferenceState> returnedObjectClassMap;


    static final String s_returnedObjectsClassMapKey = "ReturnedObject";
    static final String s_returnedObjectsIsOwnedMapKey = "ReturnedObjectIsOwned";

    final byte[] FALSE_BYTE = {0};
    final byte[] TRUE_BYTE = {1};

    private ChaincodeStub stub;
    UUIDFactory uuidFactory;

    public SerializationState() {
        guidMap = new HashMap<String, ObsidianSerialized>();
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

    // If the returned reference is owned, record that so that we can re-claim ownership when we see the object again.
    public void mapReturnedObject(ObsidianSerialized obj, boolean returnedReferenceIsOwned) {
        if (returnedObjectClassMap == null) {
            returnedObjectClassMap = new HashMap<String, ReturnedReferenceState>();
        }

        System.out.println("mapReturnedObject: " + obj.__getGUID());
        returnedObjectClassMap.put(obj.__getGUID(), new ReturnedReferenceState(obj.getClass(), returnedReferenceIsOwned));
    }

    public ReturnedReferenceState getReturnedReferenceState(ChaincodeStub stub, String guid) {
        loadReturnedObjectsMap(stub);

        return returnedObjectClassMap.get(guid);
    }

    public void archiveReturnedObjectsMap (ChaincodeStub stub) {
        // TODO: remove old, stale entries?
        System.out.println("archiveReturnedObjectsMap");

        if (returnedObjectClassMap != null) {
            for (Map.Entry<String, ReturnedReferenceState> entry : returnedObjectClassMap.entrySet()) {
                CompositeKey classKey = stub.createCompositeKey(s_returnedObjectsClassMapKey, entry.getKey());
                stub.putStringState(classKey.toString(), entry.getValue().getClassRef().getCanonicalName());
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
                    Class c = Class.forName(kv.getStringValue());

                    CompositeKey isOwnedKey = stub.createCompositeKey(s_returnedObjectsIsOwnedMapKey, kv.getKey());
                    byte[] isOwnedByteArray = stub.getState(isOwnedKey.toString());
                    boolean isOwned = (isOwnedByteArray == TRUE_BYTE) ? true : false;

                    returnedObjectClassMap.put(kv.getKey(), new ReturnedReferenceState(c, isOwned));
                    System.out.println("loading map: " + kv.getKey() + " -> " + c);
                }
                catch (ClassNotFoundException e) {
                    System.err.println("Failed to find a Class object for class name " + kv.getValue());
                }
            }
        }
    }

    public UUIDFactory getUUIDFactory() {
        return uuidFactory;
    }


    // If we are loading an object and plan to take ownership, ensure this is allowed (and record that ownership has been taken).
    public ObsidianSerialized loadContractWithGUID(ChaincodeStub stub, String guid, boolean requireOwnership) throws BadArgumentException, IllegalOwnershipConsumptionException {
        ObsidianSerialized receiverContract = getEntry(guid);

        ReturnedReferenceState refState = getReturnedReferenceState(stub, guid);
        if (refState == null) {
            System.err.println("Unable to find class of object to look up: " + guid);
            return null;
        } else {
            if (requireOwnership) {
                boolean outstandingOwnedReference = refState.getIsOwnedReference();
                if (!outstandingOwnedReference) {
                    throw new IllegalOwnershipConsumptionException(guid);
                }
                else {
                    // We've now consumed ownership, so record that fact.
                    // We can't remove the object from the map because the client may well still have a reference.
                    mapReturnedObject(receiverContract, false);
                }
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
