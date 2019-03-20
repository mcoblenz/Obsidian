package edu.cmu.cs.obsidian.chaincode;

/**
 * Created by mcoblenz on 3/19/19.
 */
public class InvalidStateException extends Exception {
    private String currentState;
    private String requestedTransaction;

    public InvalidStateException(String currentState, String requestedTransaction) {
        this.currentState = currentState;
        this.requestedTransaction = requestedTransaction;
    }

    public String toString() {
        return "Cannot invoke transaction " + requestedTransaction + " in state " + currentState + ".";
    }
}
