package edu.cmu.cs.obsidian.chaincode;

import java.util.Set;
import com.google.protobuf.InvalidProtocolBufferException;

public interface ObsidianSerialized {
    String __getGUID();
    Set<ObsidianSerialized> __resetModified(Set<ObsidianSerialized> checked);
    byte[] __archiveBytes();
    public abstract byte[] run(SerializationState st, String transactionName, byte[][] args)
            throws InvalidProtocolBufferException, ReentrancyException,
            BadTransactionException, NoSuchTransactionException;
    public abstract byte[] init(SerializationState st, byte[][] args)
            throws InvalidProtocolBufferException;

    public void flush();
}
