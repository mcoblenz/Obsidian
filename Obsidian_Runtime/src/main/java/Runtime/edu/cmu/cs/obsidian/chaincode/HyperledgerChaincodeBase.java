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
import java.util.List;

import org.hyperledger.fabric.shim.ChaincodeBase;
import org.hyperledger.fabric.shim.ChaincodeStub;
import org.hyperledger.fabric.shim.ledger.*;
import edu.cmu.cs.obsidian.chaincode.ObsidianSerialized;
import edu.cmu.cs.obsidian.chaincode.SerializationState;


public abstract class HyperledgerChaincodeBase extends ChaincodeBase implements ObsidianSerialized {
    SerializationState serializationState;

    public HyperledgerChaincodeBase() {
        serializationState = new SerializationState();
    }

    // For cross-contract instantiation.
    public HyperledgerChaincodeBase(SerializationState s) {
        serializationState = s;
    }

    public void flush() {
        // No need to do anything because main contracts aren't lazily loaded.
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
                __saveModifiedData(stub, this);

                serializationState.flushEntries();
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
        List<byte[]> allArgs = stub.getArgs();
        List<byte[]> params = allArgs.subList(1, allArgs.size());

        int paramsConsumed = 0;

        serializationState.beginTransaction();
        if (function.equals("__instantiateOther")) {
            // Expect the first arg to be the contract name of the reciever.
            if (params.size() > 0) {
                String otherContractName = new String(params.get(0));
                String otherClassName = "org.hyperledger.fabric.example." + otherContractName;
                byte[][] restArgs;
                if (params.size() > 1) {
                    restArgs = params.subList(1, params.size()).stream().toArray(byte[][]::new);
                }
                else {
                    restArgs = new byte[0][];
                }

                try {
                    ObsidianSerialized newContract = instantiateOtherContract(otherClassName, restArgs);
                    if (newContract != null) {
                        __saveModifiedData(stub, newContract);
                        serializationState.transactionSucceeded();
                        return newSuccessResponse(newContract.__getGUID().getBytes(java.nio.charset.StandardCharsets.UTF_8));
                    } else {
                        revertState();
                        return newErrorResponse("Failed to instantiate contract " + otherContractName);
                    }
                }
                catch (BadArgumentException e) {
                    revertState();
                    return newErrorResponse("Failed to instantiate contract; arguments were invalid.");
                }
                catch (WrongNumberOfArgumentsException e) {
                    revertState();
                    return newErrorResponse("Failed to instantiate contract: " + e);
                }
                catch (ObsidianRevertException e) {
                    revertState();
                    return newErrorResponse(e.getMessage());
                }
                catch (IllegalOwnershipConsumptionException e) {
                    revertState();
                    return newErrorResponse("Failed to invoke method: a parameter that the client does not own needs to be owned.");
                }
            }
            else {
                revertState();
                return newErrorResponse("Instantiating another contract requires specifying a contract class name.");

            }
        }

        // If this invocation is really to a different contract, figure that out.
        ObsidianSerialized invocationReceiver = this;
        if (params.size() > 0) {
            String firstArg = new String(params.get(0), java.nio.charset.StandardCharsets.UTF_8);
            if (firstArg.equals("__receiver")) {
                // Expect the second arg to be the GUID of the reciever.
                if (params.size() > 1) {
                    String receiverGUID = new String(params.get(1), java.nio.charset.StandardCharsets.UTF_8);
                    boolean receiverIsOwnedAtBeginning= methodReceiverIsOwnedAtBeginning(function);
                    boolean receiverIsOwnedAtEnd = methodReceiverIsOwnedAtEnd(function);
                    try {
                        invocationReceiver = serializationState.loadContractWithGUID(stub, receiverGUID, receiverIsOwnedAtBeginning, receiverIsOwnedAtEnd);
                    }
                    catch (BadArgumentException e) {
                        revertState();
                        return newErrorResponse("Failed to load receiver contract with GUID " + receiverGUID);
                    }
                    catch (IllegalOwnershipConsumptionException e) {
                        revertState();
                        return newErrorResponse("Failed to invoke method: receiver must be owned, and the client does not own it.");
                    }

                    if (invocationReceiver == null) {
                        revertState();
                        return newErrorResponse("Failed to load receiver contract with GUID " + receiverGUID);
                    }

                    paramsConsumed = 2;
                } else {
                    revertState();
                    return newErrorResponse("Invoking on a non-main contract requires specifying a receiver.");
                }
            }
        }

        // Discard any consumed parameters.
        byte[][] paramsBytes;
        if (params.size() > paramsConsumed) {
            paramsBytes = params.subList(paramsConsumed, params.size()).stream().toArray(byte[][]::new);
        } else {
            paramsBytes = new byte[0][];
        }


        try {
            /* Try to restore ourselves (the root object) from the blockchain
             * before we invoke a transaction. (This applies if we stopped the
             * chaincode and restarted it -- we have to restore the state of
             * the root object.) */
            __restoreObject(serializationState);

            byte result[] = invocationReceiver.run(serializationState, function, paramsBytes);
            __saveModifiedData(stub, invocationReceiver);
            serializationState.transactionSucceeded();
            return newSuccessResponse(result);
        } catch (NoSuchTransactionException e) {
            /* This will be returned when calling an invalid transaction
             * from the command line -- referencing an invalid transaction
             * in the client will give a compile-time error. */
            revertState();
            return newErrorResponse("No such transaction: " + function);
        } catch (ObsidianRevertException e) {
            revertState();
            return newErrorResponse(e.getMessage());
        } catch (Throwable e) {
            System.err.println("Caught exception dispatching invocation: " + e);

            e.printStackTrace();

            // Workaround for https://jira.hyperledger.org/browse/FAB-14713
            // Otherwise, we'd return newErrorResponse(e).
            String message = e.getMessage();
            if (message == null) {
                message = e.toString();
            }
            revertState();
            return newErrorResponse(message, printStackTrace(e));
        }
    }

    private static byte[] printStackTrace(Throwable throwable) {
        if (throwable == null) return new byte[0];
        final StringWriter buffer = new StringWriter();
        throwable.printStackTrace(new PrintWriter(buffer));
        return buffer.toString().getBytes(java.nio.charset.StandardCharsets.UTF_8);
    }

    private void revertState() {
        serializationState.transactionFailed();

        // Reset the heap to the prior state by re-loading the state from the ledger.
        __unload();
        try {
            __restoreObject(serializationState);
        }
        catch (InvalidProtocolBufferException e) {
            // Unrecoverable error; something is wrong with our archive!
            System.err.println("Failed to restore previous state!");
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

    // Returns the GUID of the new instance if initialization was successful, and null otherwise.
    private ObsidianSerialized instantiateOtherContract(String contractClassName, byte[][] args) throws BadArgumentException, WrongNumberOfArgumentsException, ObsidianRevertException, IllegalOwnershipConsumptionException {
        if (!contractClassName.startsWith("org.hyperledger.fabric.example")) {
            // We don't permit looking up arbitrary Java classes for security reasons!
            return null;
        }
        try {
            Class foundClass = Class.forName(contractClassName);
            Constructor<ObsidianSerialized> constructor = foundClass.getConstructor();
            ObsidianSerialized contract = constructor.newInstance();
            try {
                contract.init(serializationState, args);
            }
            catch (InvalidProtocolBufferException e) {
                System.err.println("Unable to initialize contract: " + e);
                return null;
            }

            boolean resultIsOwned = contract.constructorReturnsOwnedReference();
            serializationState.mapReturnedObject(contract, resultIsOwned);
            return contract;

        }
        catch (ClassNotFoundException | NoSuchMethodException | InstantiationException | IllegalAccessException | InvocationTargetException e) {
            System.err.println("Unable to instantiate contract: " + e);
            return null;
        }
    }

    /* Figure out what was modified and write it out to the blockchain.
     * Only called for main transactions. */
    public void __saveModifiedData(ChaincodeStub stub, ObsidianSerialized target) {
        Set<ObsidianSerialized> dirtyFields = target.__resetModified(new HashSet<ObsidianSerialized>());
        for (ObsidianSerialized field : dirtyFields) {
            /* Find key and bytes to archive for each dirty field. */
            String archiveKey = field.__getGUID();
            String archiveValue = new String(field.__archiveBytes(), java.nio.charset.StandardCharsets.UTF_8);
            System.out.println("Saving modified data: ("+field+" @ <"+archiveKey+"> => "+archiveValue+")");
            stub.putStringState(archiveKey, archiveValue);
        }

        serializationState.archiveReturnedObjectsMap(stub);
        __unload();
    }

    // Must be overridden in generated class.
    public abstract Set<ObsidianSerialized> __resetModified(Set<ObsidianSerialized> checked);
    public abstract String __getGUID();
    public abstract byte[] run(SerializationState st, String transactionName, byte[][] args)
            throws InvalidProtocolBufferException, ReentrancyException,
                   BadTransactionException, NoSuchTransactionException, BadArgumentException, WrongNumberOfArgumentsException, InvalidStateException, ObsidianRevertException, IllegalOwnershipConsumptionException;
    public abstract byte[] init(SerializationState st, byte[][] args)
            throws InvalidProtocolBufferException, BadArgumentException, WrongNumberOfArgumentsException, ObsidianRevertException, IllegalOwnershipConsumptionException;
    public abstract HyperledgerChaincodeBase __initFromArchiveBytes(byte[] archiveBytes, SerializationState __st)
        throws InvalidProtocolBufferException;
    public abstract byte[] __archiveBytes();
    public abstract void __restoreObject(SerializationState st)
        throws InvalidProtocolBufferException;
    protected abstract void __unload();
    public abstract boolean methodReceiverIsOwnedAtBeginning(String transactionName);
    public abstract boolean methodReceiverIsOwnedAtEnd(String transactionName);
    public abstract boolean constructorReturnsOwnedReference();
}
