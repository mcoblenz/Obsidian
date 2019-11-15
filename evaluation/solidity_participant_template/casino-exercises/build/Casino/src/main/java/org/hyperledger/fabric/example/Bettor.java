package org.hyperledger.fabric.example;

import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.HashSet;
import java.util.Set;
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
import org.hyperledger.fabric.example.BettingOuterClass.BettorOrGUID;
import org.hyperledger.fabric.example.BettingOuterClass.MoneyOrGUID;
import org.hyperledger.fabric.example.InterfaceImplementerWrapperOuterClass.InterfaceImplementerWrapper;
import org.hyperledger.fabric.example.InterfaceImplementerWrapperOuterClass.InterfaceImplementerWrapper.Builder;

public class Bettor
    implements ObsidianSerialized
{
    private String __guid;
    private boolean __modified;
    private boolean __loaded;
    private boolean __constructorReturnsOwned = false;
    public Money money;
    static HashSet<String> transactionsWithOwnedReceiversAtBeginning;
    static HashSet<String> transactionsWithOwnedReceiversAtEnd;
    public boolean __isInsideInvocation = false;

    public Bettor(String __guid_) {
        __modified = false;
        __loaded = false;
        __guid = __guid_;
    }

    public Bettor(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        new_Bettor(__st);
    }

    public Bettor() {
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
        if (__moneyIsInScope()) {
            if (!checked.contains(money)) {
                result.addAll(money.__resetModified(checked));
            }
        }
        if (this.__modified) {
            result.add(this);
        }
        __modified = false;
        return result;
    }

    public boolean __moneyIsInScope()
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

    private void new_Bettor(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __constructorReturnsOwned = true;
        this.money = new Money(BigInteger.valueOf(100), __st);
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

    public Money withdrawMoney(BigInteger amount, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/betting-exercises/betting_utils.obs", 0);
            } else {
                __isInsideInvocation = true;
                Money m = this.money.splitMoney(amount, __st);
                return m;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    public void receiveMoney(Money m, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/betting-exercises/betting_utils.obs", 0);
            } else {
                __isInsideInvocation = true;
                this.money.mergeMoney(m, __st);
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    @Override
    public boolean methodReceiverIsOwnedAtBeginning(String methodName) {
        if (transactionsWithOwnedReceiversAtBeginning == null) {
            transactionsWithOwnedReceiversAtBeginning = new HashSet<String>();
            transactionsWithOwnedReceiversAtBeginning.add("withdrawMoney");
        }
        return transactionsWithOwnedReceiversAtBeginning.contains(methodName);
    }

    @Override
    public boolean methodReceiverIsOwnedAtEnd(String methodName) {
        if (transactionsWithOwnedReceiversAtEnd == null) {
            transactionsWithOwnedReceiversAtEnd = new HashSet<String>();
            transactionsWithOwnedReceiversAtBeginning.add("withdrawMoney");
        }
        return transactionsWithOwnedReceiversAtEnd.contains(methodName);
    }

    public byte[] run(SerializationState __st, String transName, byte[][] args)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, NoSuchTransactionException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        byte[] returnBytes = new byte[ 0 ] ;
        if (transName.equals("receiveMoney")) {
            if (args.length == 1) {
                byte[] decoded0 = Base64 .getDecoder().decode(args[ 0 ]);
                MoneyOrGUID archive0 = MoneyOrGUID.parseFrom(decoded0);
                Money unarchivedObj0;
                if (archive0 .getEitherCase().equals((org.hyperledger.fabric.example.BettingOuterClass.MoneyOrGUID.EitherCase.GUID))) {
                    unarchivedObj0 = ((Money) __st.loadContractWithGUID(__st.getStub(), archive0 .getGuid(), true, false));
                } else {
                    unarchivedObj0 = new Money();
                    unarchivedObj0 .initFromArchive(archive0 .getObj(), __st);
                }
                Money m = unarchivedObj0;
                this.receiveMoney(m, __st);
            } else {
                System.err.println("Wrong number of arguments in invocation.");
                throw new WrongNumberOfArgumentsException("receiveMoney", args.length, 1);
            }
        } else {
            if (transName.equals("withdrawMoney")) {
                if (args.length == 1) {
                    BigInteger unmarshalledInt0 = new BigInteger(new String(args[ 0 ], StandardCharsets.UTF_8));
                    if (unmarshalledInt0 == null) {
                        throw new BadArgumentException(new String(args[ 0 ], StandardCharsets.UTF_8));
                    }
                    BigInteger amount = unmarshalledInt0;
                    Money returnObj = this.withdrawMoney(amount, __st);
                    __st.mapReturnedObject(returnObj, true);
                    Builder returnWrapperBuilder = InterfaceImplementerWrapper.newBuilder();
                    returnWrapperBuilder.setGuid(returnObj.__getGUID());
                    returnWrapperBuilder.setClassName(returnObj.getClass().getName());
                    returnBytes = Base64 .getEncoder().encode(returnWrapperBuilder.build().toByteArray());
                } else {
                    System.err.println("Wrong number of arguments in invocation.");
                    throw new WrongNumberOfArgumentsException("withdrawMoney", args.length, 1);
                }
            } else {
                throw new NoSuchTransactionException();
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
        org.hyperledger.fabric.example.BettingOuterClass.BettorOrGUID.Builder builder = org.hyperledger.fabric.example.BettingOuterClass.BettorOrGUID.newBuilder();
        builder.setObj(this.archive());
        BettorOrGUID wrappedObject = builder.build();
        return wrappedObject.toByteArray();
    }

    public org.hyperledger.fabric.example.BettingOuterClass.Bettor archive()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.BettingOuterClass.Bettor.Builder builder = org.hyperledger.fabric.example.BettingOuterClass.Bettor.newBuilder();
        builder.setGuid(__guid);
        if (money!= null) {
            String moneyArchiveID = money.__getGUID();
            builder.setMoney(moneyArchiveID);
        }
        return builder.build();
    }

    public void initFromArchive(Object archiveIn, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.BettingOuterClass.Bettor archive = ((org.hyperledger.fabric.example.BettingOuterClass.Bettor) archiveIn);
        __guid = archive.getGuid();
        if (__moneyIsInScope()) {
            String moneyID = archive.getMoney();
            Money moneyVal = ((Money) __st.getEntry(moneyID));
            if (moneyVal!= null) {
                money = moneyVal;
            } else {
                money = new Money(moneyID);
                __st.putEntry(moneyID, money);
            }
        }
        __loaded = true;
    }

    public Bettor __initFromArchiveBytes(byte[] archiveBytes, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.BettingOuterClass.Bettor archive = org.hyperledger.fabric.example.BettingOuterClass.Bettor.parseFrom(archiveBytes);
        initFromArchive(archive, __st);
        __loaded = true;
        return this;
    }

    @Override
    public byte[] init(SerializationState __st, byte[][] args)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        if (args.length!= 0) {
            throw new com.google.protobuf.InvalidProtocolBufferException("Incorrect number of arguments to constructor.");
        }
        new_Bettor(__st);
        return new byte[ 0 ] ;
    }
}
