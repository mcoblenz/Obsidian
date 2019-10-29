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
import org.hyperledger.fabric.example.AuctionOuterClass.BidOrGUID;
import org.hyperledger.fabric.example.AuctionOuterClass.MoneyOrGUID;
import org.hyperledger.fabric.example.InterfaceImplementerWrapperOuterClass.InterfaceImplementerWrapper;
import org.hyperledger.fabric.example.InterfaceImplementerWrapperOuterClass.InterfaceImplementerWrapper.Builder;

public class Bid
    implements ObsidianSerialized
{
    private Bid.State_Bid __state;
    private Bid.State_Open __stateOpen;
    private Bid.State_Stale __stateStale;
    private String __guid;
    private boolean __modified;
    private boolean __loaded;
    private boolean __constructorReturnsOwned = false;
    public Money money;
    static HashSet<String> transactionsWithOwnedReceiversAtBeginning;
    static HashSet<String> transactionsWithOwnedReceiversAtEnd;
    public boolean __isInsideInvocation = false;

    public Bid(String __guid_) {
        __modified = false;
        __loaded = false;
        __guid = __guid_;
    }

    public Bid(Money m, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        new_Bid(m, __st);
    }

    public Bid(SerializationState __st)
        throws ObsidianRevertException
    {
        new_Bid(__st);
    }

    public Bid() {
        __modified = true;
        __loaded = false;
    }

    public Bid.State_Bid getState(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        if (__st!= null) {
            this.__restoreObject(__st);
        }
        return __state;
    }

    private void __oldStateToNull()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        if (this.getState(null) == Bid.State_Bid.Open) {
            __stateOpen = null;
        }
        if (this.getState(null) == Bid.State_Bid.Stale) {
            __stateStale = null;
        }
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

    private void new_Bid(Money m, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __constructorReturnsOwned = true;
        this.money = m;
        __modified = true;
        __oldStateToNull();
        __stateOpen = new Bid.State_Open();
        __state = Bid.State_Bid.Open;
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

    public Money getMoney(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/auction-exercises/auction.obs", 0);
            } else {
                __isInsideInvocation = true;
                Money temp = new Money(this.money.getAmount(__st), __st);
                __oldStateToNull();
                __stateStale = new Bid.State_Stale();
                __state = Bid.State_Bid.Stale;
                __modified = true;
                return temp;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    public BigInteger getAmount(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/auction-exercises/auction.obs", 0);
            } else {
                __isInsideInvocation = true;
                return this.money.getAmount(__st);
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    public void bidSurpassed(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        if (this.getState(__st)!= org.hyperledger.fabric.example.Bid.State_Bid.Open) {
            throw new InvalidStateException(this, this.getState(__st).toString(), "bidSurpassed");
        }
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/auction-exercises/auction.obs", 0);
            } else {
                __isInsideInvocation = true;
                __oldStateToNull();
                __stateStale = new Bid.State_Stale();
                __state = Bid.State_Bid.Stale;
                __modified = true;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    private void new_Bid(SerializationState __st) {
        __guid = __st.getUUIDFactory().newUUID().toString();
        money = null;
        __loaded = true;
        __modified = true;
        __st.putEntry(__guid, this);
    }

    @Override
    public boolean methodReceiverIsOwnedAtBeginning(String methodName) {
        if (transactionsWithOwnedReceiversAtBeginning == null) {
            transactionsWithOwnedReceiversAtBeginning = new HashSet<String>();
            transactionsWithOwnedReceiversAtBeginning.add("getMoney");
            transactionsWithOwnedReceiversAtBeginning.add("getAmount");
            transactionsWithOwnedReceiversAtBeginning.add("bidSurpassed");
        }
        return transactionsWithOwnedReceiversAtBeginning.contains(methodName);
    }

    @Override
    public boolean methodReceiverIsOwnedAtEnd(String methodName) {
        if (transactionsWithOwnedReceiversAtEnd == null) {
            transactionsWithOwnedReceiversAtEnd = new HashSet<String>();
            transactionsWithOwnedReceiversAtBeginning.add("getMoney");
            transactionsWithOwnedReceiversAtBeginning.add("getAmount");
            transactionsWithOwnedReceiversAtBeginning.add("bidSurpassed");
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
                BigInteger returnObj = this.getAmount(__st);
                returnBytes = returnObj.toString().getBytes(StandardCharsets.UTF_8);
            } else {
                System.err.println("Wrong number of arguments in invocation.");
                throw new WrongNumberOfArgumentsException("getAmount", args.length, 0);
            }
        } else {
            if (transName.equals("getMoney")) {
                if (args.length == 0) {
                    Money returnObj = this.getMoney(__st);
                    __st.mapReturnedObject(returnObj, true);
                    Builder returnWrapperBuilder = InterfaceImplementerWrapper.newBuilder();
                    returnWrapperBuilder.setGuid(returnObj.__getGUID());
                    returnWrapperBuilder.setClassName(returnObj.getClass().getName());
                    returnBytes = Base64 .getEncoder().encode(returnWrapperBuilder.build().toByteArray());
                } else {
                    System.err.println("Wrong number of arguments in invocation.");
                    throw new WrongNumberOfArgumentsException("getMoney", args.length, 0);
                }
            } else {
                if (transName.equals("bidSurpassed")) {
                    if (args.length == 0) {
                        this.bidSurpassed(__st);
                    } else {
                        System.err.println("Wrong number of arguments in invocation.");
                        throw new WrongNumberOfArgumentsException("bidSurpassed", args.length, 0);
                    }
                } else {
                    if (transName.equals("getState")) {
                        if (args.length == 0) {
                            returnBytes = Base64 .getEncoder().encode(this.getState(__st).name().getBytes(StandardCharsets.UTF_8));
                        } else {
                            System.err.println("Wrong number of arguments in invocation.");
                            throw new WrongNumberOfArgumentsException("getState", args.length, 0);
                        }
                    } else {
                        throw new NoSuchTransactionException();
                    }
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
        org.hyperledger.fabric.example.AuctionOuterClass.BidOrGUID.Builder builder = org.hyperledger.fabric.example.AuctionOuterClass.BidOrGUID.newBuilder();
        builder.setObj(this.archive());
        BidOrGUID wrappedObject = builder.build();
        return wrappedObject.toByteArray();
    }

    public org.hyperledger.fabric.example.AuctionOuterClass.Bid archive()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.AuctionOuterClass.Bid.Builder builder = org.hyperledger.fabric.example.AuctionOuterClass.Bid.newBuilder();
        builder.setGuid(__guid);
        if (money!= null) {
            String moneyArchiveID = money.__getGUID();
            builder.setMoney(moneyArchiveID);
        }
        if (Bid.State_Bid.Open == this.getState(null)) {
            __stateOpen.archive();
            builder.setStateOpen(__stateOpen.archive());
        }
        if (Bid.State_Bid.Stale == this.getState(null)) {
            __stateStale.archive();
            builder.setStateStale(__stateStale.archive());
        }
        return builder.build();
    }

    public void initFromArchive(Object archiveIn, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.AuctionOuterClass.Bid archive = ((org.hyperledger.fabric.example.AuctionOuterClass.Bid) archiveIn);
        __guid = archive.getGuid();
        if (archive.getStateCase().equals((org.hyperledger.fabric.example.AuctionOuterClass.Bid.StateCase.STATEOPEN))) {
            __state = Bid.State_Bid.Open;
        }
        if (archive.getStateCase().equals((org.hyperledger.fabric.example.AuctionOuterClass.Bid.StateCase.STATESTALE))) {
            __state = Bid.State_Bid.Stale;
        }
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

    public Bid __initFromArchiveBytes(byte[] archiveBytes, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.AuctionOuterClass.Bid archive = org.hyperledger.fabric.example.AuctionOuterClass.Bid.parseFrom(archiveBytes);
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
        byte[] decoded0 = Base64 .getDecoder().decode(args[ 0 ]);
        MoneyOrGUID archive0 = MoneyOrGUID.parseFrom(decoded0);
        Money unarchivedObj0;
        if (archive0 .getEitherCase().equals((org.hyperledger.fabric.example.AuctionOuterClass.MoneyOrGUID.EitherCase.GUID))) {
            unarchivedObj0 = ((Money) __st.loadContractWithGUID(__st.getStub(), archive0 .getGuid(), true, false));
        } else {
            unarchivedObj0 = new Money();
            unarchivedObj0 .initFromArchive(archive0 .getObj(), __st);
        }
        new_Bid(unarchivedObj0, __st);
        return new byte[ 0 ] ;
    }

    public enum State_Bid {
        Open,
        Stale;
    }

    public class State_Open {

        public void initFromArchive(org.hyperledger.fabric.example.AuctionOuterClass.Bid.Open archive) {
        }

        public Bid.State_Open __initFromArchiveBytes(byte[] archiveBytes)
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.AuctionOuterClass.Bid.Open archive = org.hyperledger.fabric.example.AuctionOuterClass.Bid.Open.parseFrom(archiveBytes);
            initFromArchive(archive);
            return this;
        }

        public org.hyperledger.fabric.example.AuctionOuterClass.Bid.Open archive()
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.AuctionOuterClass.Bid.Open.Builder builder = org.hyperledger.fabric.example.AuctionOuterClass.Bid.Open.newBuilder();
            return builder.build();
        }
    }

    public class State_Stale {

        public void initFromArchive(org.hyperledger.fabric.example.AuctionOuterClass.Bid.Stale archive) {
        }

        public Bid.State_Stale __initFromArchiveBytes(byte[] archiveBytes)
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.AuctionOuterClass.Bid.Stale archive = org.hyperledger.fabric.example.AuctionOuterClass.Bid.Stale.parseFrom(archiveBytes);
            initFromArchive(archive);
            return this;
        }

        public org.hyperledger.fabric.example.AuctionOuterClass.Bid.Stale archive()
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.AuctionOuterClass.Bid.Stale.Builder builder = org.hyperledger.fabric.example.AuctionOuterClass.Bid.Stale.newBuilder();
            return builder.build();
        }
    }
}
