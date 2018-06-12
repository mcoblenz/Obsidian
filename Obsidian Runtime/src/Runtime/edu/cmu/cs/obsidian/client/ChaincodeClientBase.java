package edu.cmu.cs.obsidian.client;

import org.json.JSONTokener;
import org.json.JSONWriter;

import java.net.Socket;

/**
 * Created by mcoblenz on 2/16/17.
 */
public abstract class ChaincodeClientBase extends ChaincodeClientStub {

    public abstract void invokeClientMain()
            throws java.io.IOException, ChaincodeClientAbortTransactionException, edu.cmu.cs.obsidian.chaincode.ReentrancyException, edu.cmu.cs.obsidian.chaincode.BadTransactionException;


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
        try {
            socket = new Socket(addressString, port);

            assert (socket.isConnected());
            java.io.PrintWriter socketWriter = new java.io.PrintWriter(socket.getOutputStream());
            java.io.InputStreamReader socketReader = new java.io.InputStreamReader(socket.getInputStream());
            JSONWriter jsonWriter = new JSONWriter(socketWriter);
            JSONTokener jsonTokener = new JSONTokener(socket.getInputStream());
            connectionManager = new ChaincodeClientConnectionManager(socketWriter, socketReader);
        }
        catch (java.io.IOException e) {
            System.err.println("Error: failed to connect to " + addressString + ":" + port);
            System.exit(1);
        }
        catch (java.lang.IllegalArgumentException e) {
            if (port > 65535 || port < 0) {
                System.err.println("Error: port value must be between 0 and 65535, inclusive");
            } else {
                System.err.println("Error: " + addressString + " isn't a valid address");
            }
            System.exit(1);
        }

        try {
            // We need to invoke the client's main() transaction.
            invokeClientMain();
            if (socket != null) {
                socket.close();
            }
        }
        catch (java.io.IOException e) {
            System.err.println("Error: failed to write data to server: " + e);
            System.exit(1);
        }
        catch (ChaincodeClientAbortTransactionException e) {
            System.err.println("Transaction aborted: " + e);
        }
        catch (edu.cmu.cs.obsidian.chaincode.ReentrancyException e) {
            System.err.println("Error: Reentrant call made");
        }
        catch (edu.cmu.cs.obsidian.chaincode.BadTransactionException e) {
            System.err.println("Error: call to main failed " + e);
        }
    }
}
