/**
 * Created by mcoblenz on 2/16/17.
 * A chaincode program corresponds to a server that listens for transaction requests.
 * This is the base class for such servers.
 * The main method waits for transaction requests and dispatches them.
 */

package edu.cmu.cs.obsidian.chaincode;

import java.io.*;
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

public class ChaincodeBaseMock {
}


