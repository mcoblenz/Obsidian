package edu.cmu.cs.obsidian.client;

import edu.cmu.cs.obsidian.chaincode.ObsidianRevertException;
import com.google.protobuf.InvalidProtocolBufferException;

import org.json.JSONTokener;
import org.json.JSONWriter;

import java.net.Socket;

/**
 * Created by mcoblenz on 2/16/17.
 */
public abstract class ChaincodeClientBase extends ChaincodeClientStub {

    public abstract void invokeClientMain()
            throws edu.cmu.cs.obsidian.chaincode.ObsidianRevertException, InvalidProtocolBufferException, edu.cmu.cs.obsidian.chaincode.BadTransactionException, edu.cmu.cs.obsidian.chaincode.InvalidStateException, edu.cmu.cs.obsidian.chaincode.ReentrancyException, edu.cmu.cs.obsidian.client.ChaincodeClientAbortTransactionException;

    public void delegatedMain (String args[]) {

        connectionManager = new ChaincodeClientConnectionManager(false);

        try {
            // We need to invoke the client's main() transaction.
            invokeClientMain();
        }
        catch (java.io.IOException e) {
            System.err.println("Error: failed to invoke client main: " + e);
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
        catch (Exception e) {
            System.err.println("Error: an error occured " + e);
        }
    }
}
