package edu.cmu.cs.obsidian.chaincode;

/**
 * Created by mcoblenz on 3/19/19.
 */
public class BadArgumentException extends Exception {
    private String badArgument;

    public BadArgumentException(String badArgument) {
        this.badArgument = badArgument;
    }

    public String toString() {
        return "Failed to parse argument: '" + badArgument + "'.";
    }
}
