package edu.cmu.cs.obsidian.chaincode;

/**
 * Created by Miles Baker on 7/3/18.
 */
public class ObsidianChaincodeException extends Exception {
    private final String filename;
    private final int line;

    public ObsidianChaincodeException(String f, int l) {
        filename = f;
        line = l;
    }

    public String toString() {
        return "Exception " + getClass().getName() + " occurred in file " + filename + ":" + line;
    }
}
