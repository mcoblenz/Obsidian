/**
 * Created by Miles Baker on 6/14/2018.
 * Inherits from the Hyperledger ChaincodeBase class, and holds any static Java code that
 * we might want to be common to all chaincode classes.
 */
package edu.cmu.cs.obsidian.chaincode;

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

public abstract class HyperledgerChaincodeBase extends ChaincodeBase {
    @Override
    public Response init(ChaincodeStub stub) {
        final String function = stub.getFunction();
        if (!function.equals("init")) {
            return newErrorResponse("Unknown initialization function " + function);
        }
        try {
            final String args[] = stub.getParameters().stream().toArray(String[]::new);

            byte byte_args[][] = new byte[args.length][];
            for (int i = 0; i < args.length; i++) {
                byte_args[i] = args[i].getBytes();
            }
            return newSuccessResponse(init(stub, byte_args));
        } catch (Throwable e) {
            return newErrorResponse(e);
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
            byte result[] = run(stub, function, byte_args);
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
        /* archive path */
        String archivePathString = args[0];
        java.nio.file.Path archivePath = java.nio.file.Paths.get(archivePathString);

        try {
            byte[] bytes = java.nio.file.Files.readAllBytes(archivePath);
            __initFromArchiveBytes(bytes);
        }
        catch (InvalidProtocolBufferException e) {
            // Failed to read the file. Bail.
            System.err.println(archivePath + " is a file, but does not contain a valid archive of this contract.");
            System.exit(1);
        }
        catch (IOException e) {
            // If the file didn't exist, no problem; we'll create a new instance later.
            invokeConstructor();
        }

        /* setup a hook that saves data to archive file on shutdown */
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            System.out.println("\nSaving to archive...");
            byte[] newArchiveBytes = __archiveBytes();
            try {
                java.nio.file.Files.write(archivePath, newArchiveBytes,
                        StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE);
            } catch (IOException e) {
                System.err.println("Error: failed to write state to output file at " + archivePathString + ": " + e);
                System.err.println("Data may be lost.");
                System.exit(1);
            }
        }));

        /* spin up server */
        try {
            /* Don't pass the first argument (archive path) to be processed
             * by Hyperledger. Because arguments are an array, we have to copy
             * all the remaining arguments into a new array. */
            String[] less_args = new String[args.length - 1];
            for (int i = 0; i < args.length - 1; i++) {
                less_args[i] = args[i+1];
            }
            start(less_args);
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
    public abstract HyperledgerChaincodeBase __initFromArchiveBytes(byte[] archiveBytes) throws InvalidProtocolBufferException;
    public abstract byte[] __archiveBytes();
}
