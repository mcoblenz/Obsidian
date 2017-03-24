package edu.cmu.cs.obsidian.client;

import com.oracle.javafx.jmx.json.JSONReader;
import org.json.JSONTokener;
import org.json.JSONWriter;

import java.net.Socket;

/**
 * Created by mcoblenz on 2/16/17.
 */
public abstract class ChaincodeClientBase {

    public void doTransaction(Socket socket, String transactionName, Object[] args) throws java.io.IOException {
        assert (socket.isConnected());
        java.io.PrintWriter socketWriter = new java.io.PrintWriter(socket.getOutputStream());
        JSONWriter jsonWriter = new JSONWriter(socketWriter);

        jsonWriter.key("jsonrpc");
        jsonWriter.value("2.0");

        jsonWriter.key("method");
        jsonWriter.value("invoke");

        // params
        jsonWriter.key("params");
        jsonWriter.object();
        jsonWriter.key("ctorMsg");
        jsonWriter.object();
        jsonWriter.key("function");
        jsonWriter.value(transactionName);
        jsonWriter.key("args");
        jsonWriter.array();

        for (int i = 0; i < args.length; i++) {
            jsonWriter.value(args[i]);
        }
        jsonWriter.endArray(); // args
        jsonWriter.endObject(); // ctorMsg
    }

    public abstract void invokeMain(JSONWriter jsonWriter, JSONTokener jsonTokener) throws java.io.IOException;


    public void delegatedMain (String args[]) {
        if (args.length != 2) {
            java.net.URL jar = null;
            Class currentClass = new Object() { }.getClass().getEnclosingClass();
            java.security.CodeSource src = currentClass.getProtectionDomain().getCodeSource();
            if (src != null) {
                jar = src.getLocation();
            }

            System.err.println(
                    "Usage: 'java -jar " + jar + " <address> <port>' where\n" +
                            "<address> is the address of a chaincode server\n" +
                            "<port> is a port number on the chaincode server"
            );
            System.exit(1);
        }

        String addressString = args[0];
        String portString = args[1];

        int port = 0;
        try {
            port = Integer.parseInt(portString);
        }
        catch (NumberFormatException e) {
            System.err.println("Error: unable to parse " + portString + " as a port number.");
            System.exit(1);
        }

        Socket socket = null;
        JSONWriter jsonWriter = null;
        JSONTokener jsonTokener = null;
        try {
            socket = new Socket(addressString, port);

            assert (socket.isConnected());
            java.io.PrintWriter socketWriter = new java.io.PrintWriter(socket.getOutputStream());
            jsonWriter = new JSONWriter(socketWriter);
            jsonTokener = new JSONTokener(socket.getInputStream());
        }
        catch (java.io.IOException e) {
            System.err.println("Error: failed to connect to " + addressString + ":" + port);
            System.exit(1);
        }

        try {
            // We need to invoke the client's main() transaction.

            invokeMain(jsonWriter, jsonTokener);
        }

        catch (java.io.IOException e) {
            System.err.println("Error: failed to write data to server: " + e);
            System.exit(1);
        }
    }
}
