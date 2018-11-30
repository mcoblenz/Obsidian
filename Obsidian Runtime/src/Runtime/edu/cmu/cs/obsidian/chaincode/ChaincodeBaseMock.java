/**
 * Created by mcoblenz on 2/16/17.
 * A chaincode program corresponds to a server that listens for transaction requests.
 * ChaincodeBaseServer is the implementation of that server.
 * The main method waits for transaction requests and dispatches them.
 *
 * ChaincodeBaseMock is the base class for all chaincode objects on the mock environment.
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
import java.util.Map;
import java.util.HashMap;
import java.lang.ref.WeakReference;


final class ChaincodeBaseServer {
    private final int port;
    private final ChaincodeBaseMainMock base;
    private final boolean printDebug;

    private static ChaincodeBaseServer globalInstance;

    public static ChaincodeBaseServer getGlobalInstance() {
        return globalInstance;
    }

    // The server is responsible for tracking all publicly-accessible objects by GUID and dispatching invocations to the appropriate run() method.
    /* We use WeakReferences here because we still want
     * the objects to be garbage collected if they go out
     * of scope. */
    private Map<String, WeakReference<ChaincodeBaseMock>> guidMap;

    public ChaincodeBaseMock getReturnedObject(String guid) {
        WeakReference<ChaincodeBaseMock> ref = guidMap.get(guid);
        if (ref != null) {
            return ref.get();
        } else {
            return null;
        }
    }

    public void mapReturnedObject(ChaincodeBaseMock obj) {
        guidMap.put(obj.__getGUID(), new WeakReference<ChaincodeBaseMock>(obj));
    }

    public ChaincodeBaseServer(int port, ChaincodeBaseMainMock base, boolean printDebug) {
        this.port = port;
        this.base = base;
        this.printDebug = printDebug;
        guidMap = new HashMap<String, WeakReference<ChaincodeBaseMock>>();
        ChaincodeBaseServer.globalInstance = this;
    }

    // TODO: change this to gRPC.
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
     * Throws [ClassCastException] when the request in incorrectly formatted,
     * i.e. json parsing doesn't work as expected */
    private void handleRequest(ServerSocket serverSk)
            throws ClassCastException {
        Socket clientSk = null;
        try {
            clientSk = serverSk.accept();
        }
        catch (IOException e) {
            // If the client failed to establish a connection, drop this request.
            return;
        }

        if (printDebug) System.out.println("Accepted connection...");

        boolean anotherTransaction = true;
        while (!clientSk.isClosed() && anotherTransaction) {
            try {
                anotherTransaction = processTransaction(clientSk);
            }
            catch (IOException e) {
                // Bail.
                try {
                    clientSk.close();
                    return;
                }
                catch (IOException e2) {
                    // Nothing to do.
                }
                return;
            }
        }
    }
    // returns true iff we should process another transaction from this client.
    private boolean processTransaction(Socket clientSk)
            throws IOException, ClassCastException
    {

        BufferedReader in =
                new BufferedReader(new InputStreamReader(clientSk.getInputStream()));
        OutputStreamWriter out = new OutputStreamWriter(clientSk.getOutputStream());

        JSONTokener tokener = new JSONTokener(in);
        if (!tokener.more()) {
            // The client has sent an EOF, so we're done.
            return false;
        }

        JSONObject root = new JSONObject(tokener);

        if (printDebug) System.out.println("Received transaction request. JSON:\n" + root.toString());
        String method = root.getString("method");
        JSONArray txArgsJson = root.getJSONObject("params")
                .getJSONObject("ctorMsg")
                .getJSONArray("args");

        byte[][] txArgs = new byte[txArgsJson.length()][];
        for (int i = 0; i < txArgsJson.length(); i++) {
            txArgs[i] = ChaincodeUtils.JSONStringToBytes(txArgsJson.getString(i));
        }

        byte[] retBytes = new byte[0];
        boolean abortTransaction = false;
        String failureMessage = "";

        if (method.equals("deploy")) {
            if (printDebug) System.out.println("Calling constructor...");
            retBytes = base.init(base.stub, txArgs);
        }
        else if (method.equals("invoke")) {
            /* [txName] is parsed here because "deploy" doesn't take a name */
            String txName = root.getJSONObject("params")
                    .getJSONObject("ctorMsg")
                    .getString("function");

            String receiverUUID = root.getJSONObject("params")
                    .getJSONObject("ctorMsg")
                    .getString("receiver");

            ChaincodeBaseMock receiver = base;
            if (receiverUUID != null) {
                receiver = getReturnedObject(receiverUUID);
            }

            if (printDebug) System.out.println("Calling transaction '" + txName + "'...");
            try {
                retBytes = receiver.run(base.stub, txName, txArgs);
            } catch (ReentrancyException re) {
                System.out.println("Error: Reentrant call made");
                re.printStackTrace(System.out);
                retBytes = null;
                failureMessage = ": Reentrant call made";
            } catch (BadTransactionException e) {
                System.out.println("Invalid state access");
            } catch (NoSuchTransactionException e) {
                /* This should never happen, because it won't compile if you
                 * reference a nonexistent transaction. */
                System.out.println("Invalid transaction call " + txName);
                failureMessage = ": No such transaction: " + txName;
            }
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

        if (printDebug) {
            System.out.println("Raw return bytes:");
            if (retBytes == null) {
                System.out.println("(null)");
            }
            else {
                System.out.println(Arrays.toString(retBytes));
            }
        }

        JSONObject retObject = new JSONObject();
        JSONObject result = new JSONObject();
        if (retBytes == null) {
            result.put("status", "Failure" + failureMessage);
        }
        else {
            result.put("status", "OK");
            result.put("message", ChaincodeUtils.bytesToJSONString(retBytes));
        }
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
        return true;
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
    public java.util.UUID __guid = java.util.UUID.randomUUID();

    public String __getGUID() {
        return __guid.toString();
    }

    public ChaincodeBaseMock getReturnedObject(String guid) {
        return ChaincodeBaseServer.getGlobalInstance().getReturnedObject(guid);
    }

    public void mapReturnedObject(ChaincodeBaseMock obj) {
        ChaincodeBaseServer.getGlobalInstance().mapReturnedObject(obj);
    }

    public abstract byte[] run(ChaincodeStubMock stub, String transactionName, byte[][] args)
            throws InvalidProtocolBufferException, ReentrancyException,
            BadTransactionException, NoSuchTransactionException;
}



