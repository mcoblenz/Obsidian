package edu.cmu.cs.obsidian.chaincode;

import java.util.UUID;
import java.nio.ByteBuffer;

public class UUIDFactory {
    // We store a hash of the transaction ID rather than the whole ID to limit its length to something predictable.
    private int transactionIDHash;
    private int nextSerialNumber;

    public UUIDFactory (String transactionID) {
        this.transactionIDHash = transactionID.hashCode();
        nextSerialNumber = 0;
    }

    public UUID newUUID() {
        ByteBuffer b = ByteBuffer.allocate(8);
        b.putInt(transactionIDHash);
        b.putInt(nextSerialNumber);
        nextSerialNumber++;

        return UUID.nameUUIDFromBytes(b.array());
    }

}