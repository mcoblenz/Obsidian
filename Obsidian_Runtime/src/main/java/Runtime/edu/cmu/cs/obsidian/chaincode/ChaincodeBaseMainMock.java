/**
 * Created by mcoblenz on 12/4/2018.
 *
 * This is the superclass for main client contracts.
 * Non-main client contracts instead inherit from ChaincodeBaseMock.
 */



package edu.cmu.cs.obsidian.chaincode;

import com.google.protobuf.InvalidProtocolBufferException;
import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.Base64;
import java.util.Map;
import java.util.HashMap;
import java.lang.ref.WeakReference;


public abstract class ChaincodeBaseMainMock extends ChaincodeBaseMock {
    // The stub simulates the Fabric chaincode stub, which interfaces with the chaincode environment.
    public final ChaincodeStubMock stub = new ChaincodeStubMock();

    protected void invokeConstructor() {}; //to be overidden in generated code

    public void delegatedMain(String args[]) {
        if (args.length != 2) {
            java.net.URL jar = null;
            Class currentClass = new Object() { }.getClass().getEnclosingClass();
            java.security.CodeSource src = currentClass.getProtectionDomain().getCodeSource();
            if (src != null) {
                jar = src.getLocation();
            }

            System.err.println(
                    "Usage: 'java -jar " + jar + " <path> <port>' where\n" +
                    "<path> is the path to the chaincode archive\n" +
                    "<port> is a port number to listen for transactions on"
            );
            System.exit(1);
        }

        /* archive path */
        String archivePathString = args[0];
        java.nio.file.Path archivePath = java.nio.file.Paths.get(archivePathString);

        /* port */
        int port = Integer.parseInt(args[1]);

        /* TODO: add an option to turn this on/off */
        boolean printDebug = true;

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
            new ChaincodeBaseServer(port, this, printDebug).start();
        }
        catch (IOException e) {
            System.out.println("Error: IOException raised when running server: " + e);
            System.exit(1);
        }
    }

    // Must be overridden in generated class.
    public abstract byte[] init(ChaincodeStubMock stub, byte[][] args)
            throws InvalidProtocolBufferException;
    public abstract ChaincodeBaseMock __initFromArchiveBytes(byte[] archiveBytes) throws InvalidProtocolBufferException;
    public abstract byte[] __archiveBytes();
}
