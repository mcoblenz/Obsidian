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

import org.hyperledger.fabric.shim.ChaincodeBase;
import org.hyperledger.fabric.shim.ChaincodeStub;

public abstract class HyperledgerStrictChaincodeBase extends ChaincodeBase {
    @Override
    public Response init(ChaincodeStub stub) {
        final String function = stub.getFunction();
        if (function.equals("init")) {
            try {
                final String args[] = stub.getParameters().stream().toArray(String[]::new);

                byte byte_args[][] = new byte[args.length][];
                for (int i = 0; i < args.length; i++) {
                    byte_args[i] = args[i].getBytes();
                }
                byte[] result = init(stub, byte_args);
                byte worldState[] = __archiveBytes();
                stub.putStringState("obsState", new String(worldState));
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
        final String function = stub.getFunction();
        final String args[] = stub.getParameters().stream().toArray(String[]::new);

        byte byte_args[][] = new byte[args.length][];
        for (int i = 0; i < args.length; i++) {
            byte_args[i] = args[i].getBytes();
        }

        try {
            byte[] bytes = stub.getStringState("obsState").getBytes(UTF_8);
            __initFromArchiveBytes(bytes);
        } catch (InvalidProtocolBufferException e) {
            // Failed to deserialize. Bail.
            return newErrorResponse("Blockchain does not contain a valid archive of this contract.");
        } catch (Throwable e) {
            return newErrorResponse(e);
        }

        try {
            byte result[] = run(stub, function, byte_args);
            byte worldState[] = __archiveBytes();
            stub.putStringState("obsState", new String(worldState));
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

    protected void invokeConstructor() {}; //to be overidden in generated code

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

    // Must be overridden in generated class.
    public abstract byte[] init(ChaincodeStub stub, byte[][] args)
            throws InvalidProtocolBufferException;
    public abstract byte[] run(ChaincodeStub stub, String transactionName, byte[][] args)
            throws InvalidProtocolBufferException, ReentrancyException,
                   BadTransactionException, NoSuchTransactionException;
    public abstract HyperledgerStrictChaincodeBase __initFromArchiveBytes(byte[] archiveBytes)
        throws InvalidProtocolBufferException;
    public abstract byte[] __archiveBytes();
}
