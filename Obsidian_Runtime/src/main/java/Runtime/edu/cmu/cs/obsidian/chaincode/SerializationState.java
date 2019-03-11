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



public class SerializationState {
    private Map<String, ObsidianSerialized> guidMap;

    /* When we return an object to the client, we have to be able to look up that object later by its GUID.
     * On lookup, we need to be able to create a lazily-initialized object, which requires that we know the name of its class.
     *
     * */
    private Map<String, Class> returnedObjectClassMap;

    static final String s_returnedObjectsMapKey = "ReturnedObject";


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

    public void mapReturnedObject(ObsidianSerialized obj) {
        if (returnedObjectClassMap == null) {
            returnedObjectClassMap = new HashMap<String, Class>();
        }

        returnedObjectClassMap.put(obj.__getGUID(), obj.getClass());
    }

    public Class getReturnedObjectClass(ChaincodeStub stub, String guid) {
        loadReturnedObjectsMap(stub);

        return returnedObjectClassMap.get(guid);
    }

    public void archiveReturnedObjectsMap (ChaincodeStub stub) {
        // TODO: remove old, stale entries?


        for (Map.Entry<String, Class> entry : returnedObjectClassMap.entrySet()) {
            CompositeKey key = stub.createCompositeKey(s_returnedObjectsMapKey, entry.getKey());
            stub.putStringState(key.toString(), entry.getValue().getCanonicalName());
            System.out.println("archiving returned object: " + key + "->" + entry.getValue().getCanonicalName());
        }
    }

    private void loadReturnedObjectsMap (ChaincodeStub stub) {
        if (returnedObjectClassMap == null) {
            returnedObjectClassMap = new HashMap<String, Class>();

            QueryResultsIterator<KeyValue> results = stub.getStateByPartialCompositeKey(s_returnedObjectsMapKey);

            for (KeyValue kv : results) {
                try {
                    Class c = Class.forName(kv.getStringValue());
                    returnedObjectClassMap.put(kv.getKey(), c);
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
}
