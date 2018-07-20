package edu.cmu.cs.obsidian.client;


import org.hyperledger.fabric.sdk.Channel;


import org.json.JSONObject;
import org.json.JSONTokener;
import org.json.JSONWriter;

import java.util.ArrayList;
import java.util.Base64;
import java.io.*;

/**
 * Created by mcoblenz on 7/19/2018.
 */
public class HyperledgerClientConnectionManager {
    // Manages one channel and potentially multiple remote contracts.
    protected Channel channel;

    public HyperledgerClientConnectionManager (Channel c) {
        channel = c;
        printDebug = true;
    }


    // TODO: everything below here.
    protected Writer underlyingWriter;
    protected Reader underlyingReader;

    private final boolean printDebug;

    public HyperledgerClientConnectionManager(Writer w, Reader r, boolean printDebug) {
        this.underlyingWriter = w;
        this.underlyingReader = r;
        this.printDebug = printDebug;
    }

    public byte[] doTransaction(String transactionName, ArrayList<byte[]> args, boolean returnsNonvoid)
            throws java.io.IOException,
                ChaincodeClientTransactionFailedException,
                ChaincodeClientTransactionBugException
    {
        JSONWriter jsonWriter = new JSONWriter(underlyingWriter);
        if (printDebug) System.out.println("doTransaction: " + transactionName);
        jsonWriter.object(); // outer object for whole message
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
                        byte[] bytes = args.get(i);
                        String byteString = Base64.getEncoder().encodeToString(bytes);
                        jsonWriter.value(byteString);
                    }
                jsonWriter.endArray(); // args
            jsonWriter.endObject(); // ctorMsg
        jsonWriter.endObject(); // params

        jsonWriter.endObject(); // outer object
        underlyingWriter.write(jsonWriter.toString());
        underlyingWriter.flush();

        JSONTokener jsonTokener = new JSONTokener(underlyingReader);
        Object reply = jsonTokener.nextValue();
        if (printDebug) System.out.println("Received from server: " + reply);

        if (reply instanceof JSONObject) {
            JSONObject jsonReply = (JSONObject)reply;
            Object resultReply = jsonReply.get("result");
            if (resultReply instanceof JSONObject) {
                JSONObject resultJSONObject = (JSONObject)resultReply;
                String statusReply = resultJSONObject.getString("status");
                if (!statusReply.equals("OK")) {
                    throw new ChaincodeClientTransactionFailedException();
                }

                if (returnsNonvoid) {
                    Object messageReply = resultJSONObject.get("message");
                    if (messageReply instanceof String) {
                        return Base64.getDecoder().decode((String)messageReply);
                    } else {
                        // bad status type
                        throw new ChaincodeClientTransactionBugException();
                    }
                }
            }
            else {
                // bad status type
                throw new ChaincodeClientTransactionBugException();
            }


        } else {
            // bad status type
            throw new ChaincodeClientTransactionBugException();
        }

        // unreachable
        return null;
    }
}
