package edu.cmu.cs.obsidian.chaincode;

public class ReentrancyException extends ObsidianChaincodeException {
    public ReentrancyException(String f, int l) {
        super (f, l);
    }
}
