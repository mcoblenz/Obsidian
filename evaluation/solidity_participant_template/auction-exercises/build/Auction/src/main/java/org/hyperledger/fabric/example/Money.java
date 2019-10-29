package org.hyperledger.fabric.example;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.HashSet;
import java.util.Set;
import com.google.protobuf.ByteString;
import edu.cmu.cs.obsidian.chaincode.BadArgumentException;
import edu.cmu.cs.obsidian.chaincode.BadTransactionException;
import edu.cmu.cs.obsidian.chaincode.IllegalOwnershipConsumptionException;
import edu.cmu.cs.obsidian.chaincode.InvalidStateException;
import edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException;
import edu.cmu.cs.obsidian.chaincode.ObsidianRevertException;
import edu.cmu.cs.obsidian.chaincode.ObsidianSerialized;
import edu.cmu.cs.obsidian.chaincode.ReentrancyException;
import edu.cmu.cs.obsidian.chaincode.SerializationState;
import edu.cmu.cs.obsidian.chaincode.StateLockException;
import edu.cmu.cs.obsidian.chaincode.WrongNumberOfArgumentsException;
import org.hyperledger.fabric.example.Auction-solnativeOuterClass.MoneyOrGUID;
import org.hyperledger.fabric.example.InterfaceImplementerWrapperOuterClass.InterfaceImplementerWrapper;
import org.hyperledger.fabric.example.InterfaceImplementerWrapperOuterClass.InterfaceImplementerWrapper.Builder;

public class Money
    implements ObsidianSerialized
{
    private String __guid;
    private boolean __modified;
    private boolean __loaded;
    private boolean __constructorReturnsOwned = false;
    public java.math.BigInteger amount = java.math.BigInteger.valueOf(0);
    static HashSet<String> transactionsWithOwnedReceiversAtBeginning;
    static HashSet<String> transactionsWithOwnedReceiversAtEnd;
    public boolean __isInsideInvocation = false;

    public Money(String __guid_) {
        __modified = false;
        __loaded = false;
        __guid = __guid_;
    }

    public Money(java.math.BigInteger amt, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        new_Money(amt, __st);
    }

    public Money(SerializationState __st)
        throws ObsidianRevertException
    {
        new_Money(__st);
    }

    public Money() {
        __modified = true;
        __loaded = false;
    }

    public String __getGUID() {
        return __guid;
    }

    public Set<ObsidianSerialized> __resetModified(Set<ObsidianSerialized> checked)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        checked.add(this);
        Set<ObsidianSerialized> result = new HashSet<ObsidianSerialized>();
        if (!__loaded) {
            return result;
        }
        if (this.__modified) {
            result.add(this);
        }
        __modified = false;
        return result;
    }

    public boolean __amountIsInScope()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        return true;
    }

    public void __restoreObject(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        if (!__loaded) {
            String __archive_string = __st.getStub().getStringState(__guid);
            byte[] __archive_bytes = __archive_string.getBytes();
            __initFromArchiveBytes(__archive_bytes, __st);
            __loaded = true;
        }
    }

    public void flush() {
        __loaded = false;
    }

    private void new_Money(java.math.BigInteger amt, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __constructorReturnsOwned = true;
        this.amount = amt;
        __modified = true;
        __guid = __st.getUUIDFactory().newUUID().toString();
        __modified = true;
        __loaded = true;
        __st.putEntry(__guid, this);
    }

    @Override
    public boolean constructorReturnsOwnedReference() {
        return __constructorReturnsOwned;
    }

    public void mergeMoney(Money m, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/solidity_participant_template/auction-exercises/auction-solnative.obs", 0);
            } else {
                __isInsideInvocation = true;
                this.amount = this.amount.add(m.getAmount(__st));
                __modified = true;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    public java.math.BigInteger getAmount(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/solidity_participant_template/auction-exercises/auction-solnative.obs", 0);
            } else {
                __isInsideInvocation = true;
                return this.amount;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    public Money splitMoney(java.math.BigInteger amt, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/solidity_participant_template/auction-exercises/auction-solnative.obs", 0);
            } else {
                __isInsideInvocation = true;
                if (amt.compareTo(this.amount) == 1) {
                    throw new ObsidianRevertException("/Users/mcoblenz/code/Obsidian/evaluation/solidity_participant_template/auction-exercises/auction-solnative.obs", 87, "Can't split out more money than is available in a given Money object.");
                } else {
                    this.amount = this.amount.subtract(amt);
                    __modified = true;
                    return new Money(amt, __st);
                }
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    private void new_Money(SerializationState __st) {
        __guid = __st.getUUIDFactory().newUUID().toString();
        amount = java.math.BigInteger.valueOf(0);
        __loaded = true;
        __modified = true;
        __st.putEntry(__guid, this);
    }

    @Override
    public boolean methodReceiverIsOwnedAtBeginning(String methodName) {
        if (transactionsWithOwnedReceiversAtBeginning == null) {
            transactionsWithOwnedReceiversAtBeginning = new HashSet<String>();
            transactionsWithOwnedReceiversAtBeginning.add("mergeMoney");
            transactionsWithOwnedReceiversAtBeginning.add("splitMoney");
        }
        return transactionsWithOwnedReceiversAtBeginning.contains(methodName);
    }

    @Override
    public boolean methodReceiverIsOwnedAtEnd(String methodName) {
        if (transactionsWithOwnedReceiversAtEnd == null) {
            transactionsWithOwnedReceiversAtEnd = new HashSet<String>();
            transactionsWithOwnedReceiversAtBeginning.add("mergeMoney");
            transactionsWithOwnedReceiversAtBeginning.add("splitMoney");
        }
        return transactionsWithOwnedReceiversAtEnd.contains(methodName);
    }

    public byte[] run(SerializationState __st, String transName, byte[][] args)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, NoSuchTransactionException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        byte[] returnBytes = new byte[ 0 ] ;
        if (transName.equals("getAmount")) {
            if (args.length == 0) {
                java.math.BigInteger returnObj = this.getAmount(__st);
                returnBytes = returnObj.toString().getBytes(StandardCharsets.UTF_8);
            } else {
                System.err.println("Wrong number of arguments in invocation.");
                throw new WrongNumberOfArgumentsException("getAmount", args.length, 0);
            }
        } else {
            if (transName.equals("splitMoney")) {
                if (args.length == 1) {
                    java.math.BigInteger unmarshalledInt0 = new java.math.BigInteger(new String(args[ 0 ], StandardCharsets.UTF_8));
                    if (unmarshalledInt0 == null) {
                        throw new BadArgumentException(new String(args[ 0 ], StandardCharsets.UTF_8));
                    }
                    java.math.BigInteger amt = unmarshalledInt0;
                    Money returnObj = this.splitMoney(amt, __st);
                    __st.mapReturnedObject(returnObj, true);
                    Builder returnWrapperBuilder = InterfaceImplementerWrapper.newBuilder();
                    returnWrapperBuilder.setGuid(returnObj.__getGUID());
                    returnWrapperBuilder.setClassName(returnObj.getClass().getName());
                    returnBytes = Base64 .getEncoder().encode(returnWrapperBuilder.build().toByteArray());
                } else {
                    System.err.println("Wrong number of arguments in invocation.");
                    throw new WrongNumberOfArgumentsException("splitMoney", args.length, 1);
                }
            } else {
                if (transName.equals("mergeMoney")) {
                    if (args.length == 1) {
                        byte[] decoded0 = Base64 .getDecoder().decode(args[ 0 ]);
                        MoneyOrGUID archive0 = MoneyOrGUID.parseFrom(decoded0);
                        Money unarchivedObj0;
                        if (archive0 .getEitherCase().equals((org.hyperledger.fabric.example.Auction-solnativeOuterClass.MoneyOrGUID.EitherCase.GUID))) {
                            unarchivedObj0 = ((Money) __st.loadContractWithGUID(__st.getStub(), archive0 .getGuid(), true, false));
                        } else {
                            unarchivedObj0 = new Money();
                            unarchivedObj0 .initFromArchive(archive0 .getObj(), __st);
                        }
                        Money m = unarchivedObj0;
                        this.mergeMoney(m, __st);
                    } else {
                        System.err.println("Wrong number of arguments in invocation.");
                        throw new WrongNumberOfArgumentsException("mergeMoney", args.length, 1);
                    }
                } else {
                    throw new NoSuchTransactionException();
                }
            }
        }
        return returnBytes;
    }

    @Override
    public byte[] __archiveBytes()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        return this.archive().toByteArray();
    }

    @Override
    public byte[] __wrappedArchiveBytes()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.Auction-solnativeOuterClass.MoneyOrGUID.Builder builder = org.hyperledger.fabric.example.Auction-solnativeOuterClass.MoneyOrGUID.newBuilder();
        builder.setObj(this.archive());
        MoneyOrGUID wrappedObject = builder.build();
        return wrappedObject.toByteArray();
    }

    public org.hyperledger.fabric.example.Auction-solnativeOuterClass.Money archive()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.Auction-solnativeOuterClass.Money.Builder builder = org.hyperledger.fabric.example.Auction-solnativeOuterClass.Money.newBuilder();
        builder.setGuid(__guid);
        if (amount!= null) {
            builder.setAmount(ByteString.copyFrom(amount.toByteArray()));
        }
        return builder.build();
    }

    public void initFromArchive(Object archiveIn, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.Auction-solnativeOuterClass.Money archive = ((org.hyperledger.fabric.example.Auction-solnativeOuterClass.Money) archiveIn);
        __guid = archive.getGuid();
        if (__amountIsInScope()) {
            if (!archive.getAmount().isEmpty()) {
                amount = new java.math.BigInteger(archive.getAmount().toByteArray());
            }
        }
        __loaded = true;
    }

    public Money __initFromArchiveBytes(byte[] archiveBytes, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.Auction-solnativeOuterClass.Money archive = org.hyperledger.fabric.example.Auction-solnativeOuterClass.Money.parseFrom(archiveBytes);
        initFromArchive(archive, __st);
        __loaded = true;
        return this;
    }

    @Override
    public byte[] init(SerializationState __st, byte[][] args)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        if (args.length!= 1) {
            throw new com.google.protobuf.InvalidProtocolBufferException("Incorrect number of arguments to constructor.");
        }
        java.math.BigInteger unmarshalledInt0 = new java.math.BigInteger(new String(args[ 0 ], StandardCharsets.UTF_8));
        if (unmarshalledInt0 == null) {
            throw new BadArgumentException(new String(args[ 0 ], StandardCharsets.UTF_8));
        }
        new_Money(unmarshalledInt0, __st);
        return new byte[ 0 ] ;
    }
}
