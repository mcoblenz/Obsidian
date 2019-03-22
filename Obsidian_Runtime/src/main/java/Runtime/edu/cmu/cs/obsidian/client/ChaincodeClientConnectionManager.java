package edu.cmu.cs.obsidian.client;

import org.json.JSONObject;
import org.json.JSONTokener;
import org.json.JSONWriter;

import java.util.ArrayList;
import java.util.UUID;
import java.util.Base64;
import java.io.*;

/**
 * Created by mcoblenz on 4/3/17.
 */
public class ChaincodeClientConnectionManager {
    private final boolean printDebug;

    public ChaincodeClientConnectionManager(boolean printDebug) {
        this.printDebug = printDebug;
    }

    public byte[] doTransaction(String transactionName, ArrayList<byte[]> args, UUID receiverUUID, boolean returnsNonvoid)
            throws java.io.IOException,
                ChaincodeClientTransactionFailedException,
                ChaincodeClientTransactionBugException
    {
        StringJoiner sj = new StringJoiner(" ");
        sj.add(transactionName);
        sj.add("__receiver").add(receiverUUID.toString());
        for (int i = 0; i < args.size(); i++) {
            byte[] bytes = args.get(i);
            String byteString = Base64.getEncoder().encodeToString(bytes);
            sj.add(byteString);
        }

        String parameterForInvoke = sj.toString();

        Process pb = new ProcessBuilder("../network-framework/invoke.sh", parameterForInvoke);
        String output = IOUtils.toString(pb.start().getInputStream());

        return output.getBytes();
    }
}
