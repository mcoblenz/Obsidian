/**
 * Created by mcoblenz on 2/16/17.
 * A chaincode program corresponds to a server that listens for transaction requests.
 * This is the base class for such servers.
 * The main method waits for transaction requests and dispatches them.
 */

package edu.cmu.cs.obsidian.chaincode;

import com.google.protobuf.InvalidProtocolBufferException;

import java.io.*;
import java.nio.file.StandardOpenOption;
/*


class ConnectionHandler extends Thread {
    private Socket socket;

    ConnectionHandler(Socket s) {
        socket = s;
    }

    public void run() {
        try {
            BufferedReader reader = new InputStreamReader(socket.getInputStream());

        }
        catch (IOException e) {
            // Something went wrong with the socket. For now, terminate.
            socket.close();
        }

        System.out.println("hello, world");
        try {
            socket.close();
        }
        catch (IOException e) {
            // Not much we can do here.
        }
    }
}
*/

class ChaincodeBaseServer {
    private int port = 4000;
    //private Server server;
}

public abstract class ChaincodeBaseMock {
    public void delegatedMain(String args[]) {
        if (args.length < 1) {
            java.net.URL jar = null;
            Class currentClass = new Object() { }.getClass().getEnclosingClass();
            java.security.CodeSource src = currentClass.getProtectionDomain().getCodeSource();
            if (src != null) {
                jar = src.getLocation();
            }

            System.err.println("Usage: java -jar " + jar + " <path>\n   where <path> is the path to the chaincode archive.");
            System.exit(1);
        }

        String archivePathString = args[0];
        java.nio.file.Path archivePath = java.nio.file.Paths.get(archivePathString);

        try {
            byte[] bytes = java.nio.file.Files.readAllBytes(archivePath);
            initFromArchiveBytes(bytes);
        }
        catch (InvalidProtocolBufferException e) {
            // Failed to read the file. Bail.
            System.err.println(archivePath + " is a file, but does not contain a valid archive of this contract.");
            System.exit(1);
        }
        catch (IOException e) {
            // If the file didn't exist, no problem; we'll create a new instance later.
        }

        // TODO: wait for input from the network.

        // Write new state to file.

        byte[] newArchiveBytes = archiveBytes();
        try {
            java.nio.file.Files.write(archivePath, newArchiveBytes,
                    StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE);
        } catch (IOException e) {
            System.err.println("Error: failed to write state to output file at " + archivePathString + ": " + e);
            System.err.println("Data may be lost.");
            System.exit(1);
        }

    }

    // Must be overridden in generated class.
    public abstract void initFromArchiveBytes(byte[] archiveBytes) throws InvalidProtocolBufferException;
    public abstract String run(ChaincodeStubMock stub, String transactionName, String[] args);
    public abstract byte[] archiveBytes();
}


