package edu.cmu.cs.obsidian.client;

/**
 * Created by mcoblenz on 4/4/17.
 *
 * Represents transaction failure.
 *
 */
public class ChaincodeClientTransactionFailedException extends Exception {
    protected final String error;

    public ChaincodeClientTransactionFailedException(String e) {
        error = e;
    }

    public String toString() {
        return "An exception occurred: " + error;
    }
}
