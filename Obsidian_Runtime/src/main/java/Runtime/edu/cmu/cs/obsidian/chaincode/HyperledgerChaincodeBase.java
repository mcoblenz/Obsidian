/**
 * Created by Miles Baker on 6/14/2018.
 * Inherits from the Hyperledger ChaincodeBase class, and holds any static Java code that
 * we might want to be common to all chaincode classes.
 */
package edu.cmu.cs.obsidian.chaincode;

import static java.nio.charset.StandardCharsets.UTF_8;

import com.google.protobuf.InvalidProtocolBufferException;
import org.json.*;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.Base64;
import java.util.Set;
import java.util.HashSet;
import java.util.Map;
import java.util.HashMap;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import org.hyperledger.fabric.shim.ChaincodeBase;
import org.hyperledger.fabric.shim.ChaincodeStub;
import org.hyperledger.fabric.shim.ledger.*;
import edu.cmu.cs.obsidian.chaincode.ObsidianSerialized;
import edu.cmu.cs.obsidian.chaincode.SerializationState;

public abstract class HyperledgerChaincodeBase extends ChaincodeBase implements ObsidianSerialized {
    /* When we return an object to the client, we have to be able to look up that object later by its GUID.
     * On lookup, we need to be able to create a lazily-initialized object, which requires that we know the name of its class.
     *
     * */

    static final String s_returnedObjectsMapKey = "ReturnedObject";

    private Map<String, Class> returnedObjectClassMap;
    SerializationState serializationState;

    public HyperledgerChaincodeBase() {
        serializationState = new SerializationState();
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

    @Override
    public Response init(ChaincodeStub stub) {
        serializationState.setStub(stub);

        final String function = stub.getFunction();
        if (function.equals("init")) {
            try {
                final String args[] = stub.getParameters().stream().toArray(String[]::new);

                byte byte_args[][] = new byte[args.length][];
                for (int i = 0; i < args.length; i++) {
                    byte_args[i] = args[i].getBytes();
                }
                byte[] result = init(serializationState, byte_args);
                __saveModifiedData(stub);
                return newSuccessResponse(result);
            } catch (Throwable e) {
                return newErrorResponse(e);
            }
        } else {
            return newErrorResponse("Unknown initialization function " + function);
        }
    }

    @Override
    public Response invoke(ChaincodeStub stub) {
        serializationState.setStub(stub);

        final String function = stub.getFunction();
        String args[] = stub.getParameters().stream().toArray(String[]::new);

        // If this invocation is really to a different contract, figure that out.
        ObsidianSerialized invocationReceiver = this;
        if (args.length > 0) {
            String firstArg = args[0];
            if (firstArg.equals("__receiver")) {
                // Expect the second arg to be the GUID of the reciever.
                if (args.length > 1) {
                    String receiverGUID = args[1];
                    ObsidianSerialized receiverContract = serializationState.getEntry(receiverGUID);
                    // If it's not in our map, maybe we just haven't loaded it yet.
                    if (receiverContract == null) {
                        Class objectClass = getReturnedObjectClass(stub, receiverGUID);
                        try {
                            Constructor constructor = objectClass.getConstructor(String.class);
                            receiverContract = (ObsidianSerialized)constructor.newInstance(receiverGUID);
                            serializationState.putEntry(receiverGUID, receiverContract);
                        }
                        catch (NoSuchMethodException |
                                InstantiationException |
                                IllegalAccessException |
                                InvocationTargetException e) {
                            return newErrorResponse("Failed to instantiate archived object: " + e);
                        }
                    }

                    if (receiverContract == null) {
                        return newErrorResponse("Cannot invoke transaction on unknown object " + receiverGUID);
                    }
                    else {
                        if (receiverContract instanceof ObsidianSerialized) {
                            invocationReceiver = (ObsidianSerialized)receiverContract;
                        }
                        else {
                            return newErrorResponse("Cannot invoke transaction on non-contract " + receiverContract);
                        }
                    }

                    if (args.length > 2) {
                        args = Arrays.copyOfRange(args, 2, args.length - 1);
                    }
                    else {
                        args = new String[0];
                    }
                }
                else {
                    return newErrorResponse("Invoking on a non-main contract requires specifying a receiver.");
                }
            }
        }

        byte byte_args[][] = new byte[args.length][];
        for (int i = 0; i < args.length; i++) {
            byte_args[i] = args[i].getBytes();
        }

        try {
            System.out.println("before _restoreObject");
            /* Try to restore ourselves (the root object) from the blockchain
             * before we invoke a transaction. (This applies if we stopped the
             * chaincode and restarted it -- we have to restore the state of
             * the root object.) */
            __restoreObject(serializationState);
            System.out.println("after restoreObject");
            byte result[] = invocationReceiver.run(serializationState, function, byte_args);
            System.out.println("after run");
            __saveModifiedData(stub);
            System.out.println("after saveModifiedData");
            return newSuccessResponse(result);
        } catch (NoSuchTransactionException e) {
            /* This will be returned when calling an invalid transaction
             * from the command line -- referencing an invalid transaction
             * in the client will give a compile-time error. */
            return newErrorResponse("No such transaction: " + function);
        } catch (Throwable e) {
            return newErrorResponse(e);
        }
    }

    public void delegatedMain(String args[]) {
        /* spin up server */
        try {
            start(args);
        }
        catch (Exception e) {
            System.out.println("Error: Exception raised when running server: " + e);
            System.exit(1);
        }
    }

    /* Figure out what was modified and write it out to the blockchain.
     * Only called for main transactions. */
    public void __saveModifiedData(ChaincodeStub stub) {
        Set<ObsidianSerialized> dirtyFields = __resetModified(new HashSet<ObsidianSerialized>());
        for (ObsidianSerialized field : dirtyFields) {
            /* Find key and bytes to archive for each dirty field. */
            String archiveKey = field.__getGUID();
            String archiveValue = new String(field.__archiveBytes());
            System.out.println("Saving modified data: ("+field+" @ <"+archiveKey+"> => "+archiveValue+")");
            stub.putStringState(archiveKey, archiveValue);
        }
        __unload();
    }

    // Must be overridden in generated class.
    public abstract Set<ObsidianSerialized> __resetModified(Set<ObsidianSerialized> checked);
    public abstract String __getGUID();
    public abstract byte[] init(SerializationState st, byte[][] args)
            throws InvalidProtocolBufferException;
    public abstract byte[] run(SerializationState st, String transactionName, byte[][] args)
            throws InvalidProtocolBufferException, ReentrancyException,
                   BadTransactionException, NoSuchTransactionException;
    public abstract HyperledgerChaincodeBase __initFromArchiveBytes(byte[] archiveBytes, SerializationState __st)
        throws InvalidProtocolBufferException;
    public abstract byte[] __archiveBytes();
    public abstract void __restoreObject(SerializationState st)
        throws InvalidProtocolBufferException;
    protected abstract void __unload();
}
