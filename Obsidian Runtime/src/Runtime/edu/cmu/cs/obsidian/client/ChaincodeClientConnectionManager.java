package edu.cmu.cs.obsidian.client;

import org.json.JSONObject;
import org.json.JSONTokener;
import org.json.JSONWriter;

import java.util.ArrayList;
import java.util.Base64;

/**
 * Created by mcoblenz on 4/3/17.
 */
public class ChaincodeClientConnectionManager {
    protected JSONWriter jsonWriter;
    protected JSONTokener jsonTokener;

    public ChaincodeClientConnectionManager(JSONWriter jsonWriter, JSONTokener jsonTokener) {
        this.jsonWriter = jsonWriter;
        this.jsonTokener = jsonTokener;
    }

    public byte[] doTransaction(String transactionName, ArrayList<byte[]> args, boolean returnsNonvoid)
            throws java.io.IOException,
                ChaincodeClientTransactionFailedException,
                ChaincodeClientTransactionBugException
    {
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

        for (int i = 0; i < args.size(); i++) {
            jsonWriter.value(args.get(i));
        }
        jsonWriter.endArray(); // args
        jsonWriter.endObject(); // ctorMsg

        Object reply = jsonTokener.nextValue();
        System.out.println("Received from server: " + reply);
        if (reply instanceof JSONObject) {
            JSONObject jsonReply = (JSONObject)reply;
            Object statusReply = jsonReply.get("status");
            if (statusReply instanceof String) {
                if (!((String)statusReply).equals("OK")) {
                    throw new ChaincodeClientTransactionFailedException();
                }
            }
            else {
                // bad status type
                throw new ChaincodeClientTransactionBugException();
            }

            if (returnsNonvoid) {
                Object messageReply = jsonReply.get("message");
                if (messageReply instanceof String) {
                    return Base64.getDecoder().decode((String)messageReply);
                } else {
                    // bad status type
                    throw new ChaincodeClientTransactionBugException();
                }
            }
        } else {
            // bad status type
            throw new ChaincodeClientTransactionBugException();
        }

        // unreachable
        return null;
    }
}
