// This exception occurs when a program attempts to transition a statelocked object via a Shared reference.

package edu.cmu.cs.obsidian.chaincode;

public class StateLockException extends ObsidianChaincodeException {

    public StateLockException(String f, int l) {
        super (f, l);
    }

    @Override
    public String getMessage() {
        return "[" + filename + ": " + line + "]: Cannot transition object while it is being used in a dynamic state test.";
    }
}
