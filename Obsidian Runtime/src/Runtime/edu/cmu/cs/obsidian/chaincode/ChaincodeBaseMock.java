/**
 * Created by mcoblenz on 2/16/17.
 * A chaincode program corresponds to a server that listens for transaction requests.
 * This is the base class for such servers.
 * The main method waits for transaction requests and dispatches them.
 */

package edu.cmu.cs.obsidian.chaincode;

import com.google.protobuf.InvalidProtocolBufferException;
import org.json.*;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.file.StandardOpenOption;

class ChaincodeBaseServer {
    private final int port;
    private final ChaincodeBaseMock base;
    private final boolean printDebug;

    private static char[] hexCharList = {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'};

    /* always returns a length-two string */
    private static String byteToString(byte b) {
        int bAsInt = b & 0x000000ff;
        StringBuilder builder = new StringBuilder();

        /* most significant byte goes first */
        builder.append(hexCharList[bAsInt >>> 4]);

        /* least significant byte goes second */
        builder.append(hexCharList[(bAsInt << 28) >>> 28]);
        return builder.toString();
    }

    /* [c1] is least significant; [c1] and [c2] should be characters in [hexCharList] */
    private static byte charsToByte(char c1, char c2) {
        byte b = 0;
        for (int i = 0; i < 16; i++) {
            if (hexCharList[i] == c1) b |= i;
        }
        for (int i = 0; i < 16; i++) {
            if (hexCharList[i] == c2) b |= (i << 4);
        }
        return b;
    }

    private static String bytesToHexStr(byte[] bytes) {
        StringBuilder builder = new StringBuilder();
        for (byte b : bytes) builder.append(byteToString(b));
        return builder.toString();
    }

    /* hex string should not remove trailing 0's: e.g. length(hex) must be even */
    private static byte[] hexStringToBytes(String hex) {
        int length = hex.length() / 2 + (hex.length() % 2);
        byte[] bytes = new byte[length];
        for (int i = 0; i < length; i++) {
            bytes[i] = charsToByte(hex.charAt(2 * i + 1), hex.charAt(2 * i));
        }
        return bytes;
    }

    public ChaincodeBaseServer(int port, ChaincodeBaseMock base, boolean printDebug) {
        this.port = port;
        this.base = base;
        this.printDebug = printDebug;
    }

    /* Example of expected format:
        {
            "method" : "deploy" or "invoke" or "query",
            "params" : {
                "ctorMsg" : {
                    "function" : "transaction_name",
                    "args" : [protobuf messages for tx args go in this array]
                }
            },
        }

        Example response format:
        {
            "result": {
                "status": "OK",
                "message": [return value encoded]
            }
        }

     */

    /* Waits for a request and then runs a transaction based on the request.
     * Throws [IOException] when there's a networking error.
     * Throws [ClassCastException] when the request in incorrectly formatted,
     * i.e. json parsing doesn't work as expected */
    private void handleRequest(ServerSocket serverSk)
            throws IOException, ClassCastException {
        Socket clientSk = serverSk.accept();

        if (printDebug) System.out.println("Accepting transaction...");

        BufferedReader in =
                new BufferedReader(new InputStreamReader(clientSk.getInputStream()));
        OutputStreamWriter out = new OutputStreamWriter(clientSk.getOutputStream());

        JSONTokener tokener = new JSONTokener(in);
        JSONObject root = new JSONObject(tokener);

        if (printDebug) System.out.println("Received JSON:\n" + root.toString());

        String method = root.getString("method");
        JSONArray txArgsJson = root.getJSONObject("params")
                .getJSONObject("ctorMsg")
                .getJSONArray("args");

        byte[][] txArgs = new byte[txArgsJson.length()][];
        for (int i = 0; i < txArgsJson.length(); i++) {
            txArgs[i] = hexStringToBytes(txArgsJson.getString(i));
        }

        byte[] retBytes = new byte[0];

        if (method.equals("deploy")) {
            if (printDebug) System.out.println("Calling constructor...");
            retBytes = base.init(base.stub, txArgs);
        }
        else if (method.equals("invoke")) {
            /* [txName] is parsed here because "deploy" doesn't take a name */
            String txName = root.getJSONObject("params")
                    .getJSONObject("ctorMsg")
                    .getString("function");

            if (printDebug) System.out.println("Calling transaction '" + txName + "'...");
            retBytes = base.run(base.stub, txName, txArgs);
        }
        else if (method.equals("query")) {
            /* TODO : do we support queries? */
        }
        else {
            /* like other places in this method where this exception is thrown,
             * this will conceptually indicate that the JSON in malformed */
            throw new ClassCastException();
        }

        /* we should try to send the return value back, but not fail,
         * e.g. if the client closes the socket after the tx is sent */
        JSONObject retObject = new JSONObject();
        JSONObject result = new JSONObject();
        result.put("status", "OK");
        result.put("message", bytesToHexStr(retBytes));
        retObject.put("result", result);

        if (printDebug) {
            System.out.println("Sending back JSON: ");
            System.out.println(retObject.toString());
        }

        try {
            out.write(retObject.toString());
            out.flush();
            if (printDebug) System.out.println("Successfully sent return value");
        } catch (IOException e) {
            if (printDebug) System.out.println("Client rejected return value");
        }

        if (printDebug) System.out.println("Transaction completed");
        clientSk.close();
    }

    /* this ought to never return under normal conditions: it accepts connections
     * until the program is killed */
    public void start() throws IOException {
        ServerSocket serverSk = new ServerSocket(port);

        if (printDebug)
            System.out.println("Listening on port " + Integer.toString(port) + "...");

        /* transactions are executed one at a time (i.e. not concurrently),
         * and in the order they are received by the server */
        while (true) {
            try { handleRequest(serverSk); }
            /* this means the json wasn't in the right format;
             * we should continue handling requests */
            catch (ClassCastException e) {
                if (printDebug) System.out.println("Bad Request: JSON failed to parse");
            }
        }
    }
}

public abstract class ChaincodeBaseMock {
    public final ChaincodeStubMock stub = new ChaincodeStubMock();

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

        /* setup a hook that saves data to archive file on shutdown */
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            System.out.println("\nSaving to archive...");
            byte[] newArchiveBytes = archiveBytes();
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
            System.out.println("Error: IOException raised when running server");
            System.exit(1);
        }
    }

    // Must be overridden in generated class.
    public abstract byte[] init(ChaincodeStubMock stub, byte[][] args);
    public abstract byte[] run(ChaincodeStubMock stub, String transactionName, byte[][] args);

    public abstract void initFromArchiveBytes(byte[] archiveBytes) throws InvalidProtocolBufferException;
    public abstract byte[] archiveBytes();
}


