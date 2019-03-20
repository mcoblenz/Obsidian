package edu.cmu.cs.obsidian.chaincode;

/**
 * Created by Miles Baker on 7/3/18.
 */
public class ObsidianChaincodeException extends Exception {
    protected final String filename;
    protected final int line;

    public ObsidianChaincodeException(String f, int l) {
        filename = f;
        line = l;
    }

    public String toString() {
        return filename + ":" + line + ": Exception " + getClass().getName() + " occurred.";
    }
}
