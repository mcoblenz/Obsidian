package edu.cmu.cs.obsidian.chaincode;

/**
 * Created by mcoblenz on 3/19/19.
 */
public class InvalidStateException extends Exception {
    private ObsidianSerialized statefulObj;
    private String currentState;
    private String requestedTransaction;

    public InvalidStateException(ObsidianSerialized statefulObj, String currentState, String requestedTransaction) {
        this.statefulObj = statefulObj;
        this.currentState = currentState;
        this.requestedTransaction = requestedTransaction;
    }

    public String toString() {
        return "Cannot invoke transaction " + requestedTransaction + " with " + statefulObj + " in state " + currentState + ".";
    }
}
