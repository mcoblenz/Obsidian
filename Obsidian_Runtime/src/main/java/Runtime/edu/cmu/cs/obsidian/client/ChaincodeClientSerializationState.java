/**
 * Created by Miles Baker, 7/25/18.
 *
 * Holds the current serialization state of a contract.
 * It contains a map of GUIDs to already-deserialized
 * objects, and a chaincode stub to get objects out
 * of the blockchain that aren't already loaded. */
package edu.cmu.cs.obsidian.client;

import org.hyperledger.fabric.shim.ChaincodeStub;
import org.hyperledger.fabric.shim.ledger.*;

import edu.cmu.cs.obsidian.chaincode.ObsidianSerialized;
import edu.cmu.cs.obsidian.chaincode.SerializationState;
import edu.cmu.cs.obsidian.chaincode.UUIDFactory;
import java.util.Map;
import java.util.HashMap;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;




public class ChaincodeClientSerializationState extends SerializationState {

    public ChaincodeClientSerializationState() {
        setUUIDFactory(new UUIDFactory("0"));
    }

    @Override
    public void setStub(ChaincodeStub newStub) {
    }

    @Override
    public void putEntry(String guid, ObsidianSerialized obj){
    }

    @Override
    public void flushEntries() {
    }

    @Override
    public void mapReturnedObject(ObsidianSerialized obj) {
    }

    @Override
    public void archiveReturnedObjectsMap (ChaincodeStub stub) {
    }
}
