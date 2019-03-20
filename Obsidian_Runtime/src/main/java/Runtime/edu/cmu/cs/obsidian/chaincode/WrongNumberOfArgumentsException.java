package edu.cmu.cs.obsidian.chaincode;

/**
 * Created by mcoblenz on 4/6/17.
 */
public class WrongNumberOfArgumentsException extends Exception {
    private String transactionName;
    private int actualArgs;
    private int expectedArgs;

    public WrongNumberOfArgumentsException(String transactionName, int actualArgs, int expectedArgs) {
        this.transactionName = transactionName;
        this.actualArgs = actualArgs;
        this.expectedArgs = expectedArgs;
    }

    public String toString() {
        return "Wrong number of arguments invoking transaction " + transactionName +
                ". Expected " + expectedArgs + "; got " + actualArgs + ".";
    }
}
