package edu.cmu.cs.obsidian.client;

import org.json.JSONWriter;
import org.json.JSONTokener;

import java.net.Socket;

/**
 * Created by mcoblenz on 4/3/17.
 */
public abstract class ChaincodeClientStub {
    protected ChaincodeClientConnectionManager connectionManager;

    private final byte[] TRUE_ARRAY = {1};
    private final byte[] FALSE_ARRAY = {0};

    // This constructor is used AFTER main finishes setting up the connection.
    public ChaincodeClientStub(ChaincodeClientConnectionManager connectionManager) {
        this.connectionManager = connectionManager;
    }

    // This constructor is only used while configuring the main contract stub.
    ChaincodeClientStub() {
    }
}
