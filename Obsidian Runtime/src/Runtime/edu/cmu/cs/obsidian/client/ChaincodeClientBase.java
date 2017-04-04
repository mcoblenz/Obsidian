package edu.cmu.cs.obsidian.client;

import org.json.JSONTokener;
import org.json.JSONWriter;

import java.net.Socket;

/**
 * Created by mcoblenz on 2/16/17.
 */
public abstract class ChaincodeClientBase extends ChaincodeClientStub {

    public abstract void invokeClientMain() throws java.io.IOException;


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

        try {
            Socket socket = new Socket(addressString, port);

            assert (socket.isConnected());
            java.io.PrintWriter socketWriter = new java.io.PrintWriter(socket.getOutputStream());
            JSONWriter jsonWriter = new JSONWriter(socketWriter);
            JSONTokener jsonTokener = new JSONTokener(socket.getInputStream());
            connectionManager = new ChaincodeClientConnectionManager(jsonWriter, jsonTokener);
        }
        catch (java.io.IOException e) {
            System.err.println("Error: failed to connect to " + addressString + ":" + port);
            System.exit(1);
        }

        try {
            // We need to invoke the client's main() transaction.
            invokeClientMain();
        }

        catch (java.io.IOException e) {
            System.err.println("Error: failed to write data to server: " + e);
            System.exit(1);
        }
    }
}
