package edu.cmu.cs.obsidian.chaincode;

/**
 * Created by mcoblenz on 3/19/19.
 */
public class IllegalOwnershipConsumptionException extends Exception {
    String guid;

    public IllegalOwnershipConsumptionException(String guid) {
        this.guid = guid;
    }

    public String toString() {
        return "Cannot consume ownership of object " + guid +
                " because that object is already owned by another reference on the blockchain.";
    }
}