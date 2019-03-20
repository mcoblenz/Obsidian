package edu.cmu.cs.obsidian.chaincode;

/**
 * Created by mcoblenz on 4/6/17.
 */
public class BadTransactionException extends ObsidianChaincodeException {
    public BadTransactionException(String f, int l) {
        super (f, l);
    }
}
