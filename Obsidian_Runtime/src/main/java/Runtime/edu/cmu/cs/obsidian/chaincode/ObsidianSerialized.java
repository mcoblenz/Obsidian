package edu.cmu.cs.obsidian.chaincode;

import java.util.Set;
import com.google.protobuf.InvalidProtocolBufferException;

public interface ObsidianSerialized {
    String __getGUID();
    Set<ObsidianSerialized> __resetModified(Set<ObsidianSerialized> checked);
    byte[] __archiveBytes();
    public abstract byte[] run(SerializationState st, String transactionName, byte[][] args)
            throws InvalidProtocolBufferException, ReentrancyException,
            BadTransactionException, BadArgumentException, NoSuchTransactionException, WrongNumberOfArgumentsException, InvalidStateException, ObsidianRevertException, IllegalOwnershipConsumptionException;
    public abstract byte[] init(SerializationState st, byte[][] args)
            throws InvalidProtocolBufferException, BadArgumentException, WrongNumberOfArgumentsException, ObsidianRevertException, IllegalOwnershipConsumptionException;

    public void flush();
    public abstract boolean methodReceiverIsOwned(String transactionName);
    public abstract boolean constructorReturnsOwnedReference();
}
