// This represents an explicit throw statement executed in chaincode.

package edu.cmu.cs.obsidian.chaincode;

public class ObsidianThrowException extends ObsidianChaincodeException {
    String message;

    public ObsidianThrowException(String f, int l, String message) {
        super (f, l);

        this.message = message;
    }

    @Override
    public String getMessage() {
        if (message == null) {
            return "[" + filename + ": " + line + "]: Smart contract threw an exception.";
        }
        else {
            return "[" + filename + ": " + line + "]: Smart contract threw an exception: " + message;
        }
    }
}
