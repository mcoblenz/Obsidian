package edu.cmu.cs.obsidian.client;

import org.json.JSONObject;
import org.json.JSONTokener;
import org.json.JSONWriter;

import java.util.ArrayList;
import java.util.Base64;
import java.util.StringJoiner;
import org.apache.commons.io.IOUtils;

/**
 * Created by mcoblenz on 4/3/17.
 */
public class ChaincodeClientConnectionManager {
    private final boolean printDebug;

    public ChaincodeClientConnectionManager(boolean printDebug) {
        this.printDebug = printDebug;
    }

    public byte[] doTransaction(String transactionName, ArrayList<byte[]> args, String receiverUUID, boolean returnsNonvoid)
            throws java.io.IOException,
                ChaincodeClientTransactionFailedException,
                ChaincodeClientTransactionBugException
    {
        StringJoiner sj = new StringJoiner(" ");
        sj.add("-q");
        sj.add(transactionName);
        sj.add("__receiver").add(receiverUUID);
        for (int i = 0; i < args.size(); i++) {
            byte[] bytes = args.get(i);
            String byteString = Base64.getEncoder().encodeToString(bytes);
            sj.add(byteString);
        }

        String parameterForInvoke = sj.toString();
        ProcessBuilder pb = new ProcessBuilder("../../../network-framework/invoke.sh", parameterForInvoke);
        String output = IOUtils.toString(pb.start().getInputStream());
        return output.getBytes();
    }
}
