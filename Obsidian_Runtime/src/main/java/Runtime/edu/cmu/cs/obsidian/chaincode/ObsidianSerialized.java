package edu.cmu.cs.obsidian.chaincode;

import java.util.Set;
import com.google.protobuf.InvalidProtocolBufferException;

public interface ObsidianSerialized {
    String __getGUID();
    Set<ObsidianSerialized> __resetModified(Set<ObsidianSerialized> checked) throws InvalidProtocolBufferException;
    byte[] __archiveBytes() throws InvalidProtocolBufferException;
    byte[] __wrappedArchiveBytes() throws InvalidProtocolBufferException;
    public abstract byte[] run(SerializationState st, String transactionName, byte[][] args)
            throws InvalidProtocolBufferException, ReentrancyException,
                BadTransactionException, NoSuchTransactionException, BadArgumentException,
                WrongNumberOfArgumentsException, InvalidStateException, ObsidianRevertException,
                IllegalOwnershipConsumptionException, StateLockException;
    public abstract byte[] init(SerializationState st, byte[][] args)
            throws InvalidProtocolBufferException, ReentrancyException,
                BadTransactionException, NoSuchTransactionException, BadArgumentException,
                WrongNumberOfArgumentsException, InvalidStateException, ObsidianRevertException,
                IllegalOwnershipConsumptionException, StateLockException;

    public void flush();
    public abstract boolean methodReceiverIsOwnedAtBeginning(String transactionName);
    public abstract boolean methodReceiverIsOwnedAtEnd(String transactionName);
    public abstract boolean constructorReturnsOwnedReference();
}
