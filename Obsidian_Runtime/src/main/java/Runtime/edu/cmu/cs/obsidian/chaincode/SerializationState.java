/**
 * Created by Miles Baker, 7/25/18.
 *
 * Holds the current serialization state of a contract.
 * It contains a map of GUIDs to already-deserialized
 * objects, and a chaincode stub to get objects out
 * of the blockchain that aren't already loaded. */
package edu.cmu.cs.obsidian.chaincode;

import org.hyperledger.fabric.shim.ChaincodeStub;
import edu.cmu.cs.obsidian.chaincode.ObsidianSerialized;
import java.util.Map;
import java.util.HashMap;
import java.lang.ref.WeakReference;

public class SerializationState {
    /* We use WeakReferences here because we still want
     * the objects to be garbage collected if they go out
     * of scope. */
    private Map<String, WeakReference<ObsidianSerialized>> guidMap;
    private ChaincodeStub stub;
    UUIDFactory uuidFactory;

    public SerializationState() {
        guidMap = new HashMap<String, WeakReference<ObsidianSerialized>>();
    }

    public void setStub(ChaincodeStub newStub) {
        stub = newStub;
        uuidFactory = new UUIDFactory(newStub.getTxId());
    }

    public ChaincodeStub getStub() {
        return stub;
    }

    public ObsidianSerialized getEntry(String guid) {
        WeakReference<ObsidianSerialized> ref = guidMap.get(guid);
        if (ref != null) {
            return ref.get();
        } else {
            return null;
        }
    }

    public void putEntry(String guid, ObsidianSerialized obj) {
        guidMap.put(guid, new WeakReference<ObsidianSerialized>(obj));
    }

    public UUIDFactory getUUIDFactory() {
        return uuidFactory;
    }
}
