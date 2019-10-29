package org.hyperledger.fabric.example;

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
import org.hyperledger.fabric.example.BettingOuterClass.BetOrGUID;
import org.hyperledger.fabric.example.BettingOuterClass.BetPredictionOrGUID;
import org.hyperledger.fabric.example.BettingOuterClass.BettorOrGUID;
import org.hyperledger.fabric.example.InterfaceImplementerWrapperOuterClass.InterfaceImplementerWrapper;
import org.hyperledger.fabric.example.InterfaceImplementerWrapperOuterClass.InterfaceImplementerWrapper.Builder;

public class Bet
    implements ObsidianSerialized
{
    private String __guid;
    private boolean __modified;
    private boolean __loaded;
    private boolean __constructorReturnsOwned = false;
    public Bettor bettor;
    public BetPrediction prediction;
    static HashSet<String> transactionsWithOwnedReceiversAtBeginning;
    static HashSet<String> transactionsWithOwnedReceiversAtEnd;
    public boolean __isInsideInvocation = false;

    public Bet(String __guid_) {
        __modified = false;
        __loaded = false;
        __guid = __guid_;
    }

    public Bet(Bettor b, BetPrediction p, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        new_Bet(b, p, __st);
    }

    public Bet(SerializationState __st)
        throws ObsidianRevertException
    {
        new_Bet(__st);
    }

    public Bet() {
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
        if (__bettorIsInScope()) {
            if (!checked.contains(bettor)) {
                result.addAll(bettor.__resetModified(checked));
            }
        }
        if (__predictionIsInScope()) {
            if (!checked.contains(prediction)) {
                result.addAll(prediction.__resetModified(checked));
            }
        }
        if (this.__modified) {
            result.add(this);
        }
        __modified = false;
        return result;
    }

    public boolean __bettorIsInScope()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        return true;
    }

    public boolean __predictionIsInScope()
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

    private void new_Bet(Bettor b, BetPrediction p, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __constructorReturnsOwned = true;
        this.bettor = b;
        __modified = true;
        this.prediction = p;
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

    public Bettor getBettor(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/betting-exercises/betting_utils.obs", 0);
            } else {
                __isInsideInvocation = true;
                return this.bettor;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    public BetPrediction getBetPrediction(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/betting-exercises/betting_utils.obs", 0);
            } else {
                __isInsideInvocation = true;
                return this.prediction;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    private void new_Bet(SerializationState __st) {
        __guid = __st.getUUIDFactory().newUUID().toString();
        bettor = null;
        prediction = null;
        __loaded = true;
        __modified = true;
        __st.putEntry(__guid, this);
    }

    @Override
    public boolean methodReceiverIsOwnedAtBeginning(String methodName) {
        if (transactionsWithOwnedReceiversAtBeginning == null) {
            transactionsWithOwnedReceiversAtBeginning = new HashSet<String>();
        }
        return transactionsWithOwnedReceiversAtBeginning.contains(methodName);
    }

    @Override
    public boolean methodReceiverIsOwnedAtEnd(String methodName) {
        if (transactionsWithOwnedReceiversAtEnd == null) {
            transactionsWithOwnedReceiversAtEnd = new HashSet<String>();
        }
        return transactionsWithOwnedReceiversAtEnd.contains(methodName);
    }

    public byte[] run(SerializationState __st, String transName, byte[][] args)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, NoSuchTransactionException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        byte[] returnBytes = new byte[ 0 ] ;
        if (transName.equals("getBettor")) {
            if (args.length == 0) {
                Bettor returnObj = this.getBettor(__st);
                __st.mapReturnedObject(returnObj, false);
                Builder returnWrapperBuilder = InterfaceImplementerWrapper.newBuilder();
                returnWrapperBuilder.setGuid(returnObj.__getGUID());
                returnWrapperBuilder.setClassName(returnObj.getClass().getName());
                returnBytes = Base64 .getEncoder().encode(returnWrapperBuilder.build().toByteArray());
            } else {
                System.err.println("Wrong number of arguments in invocation.");
                throw new WrongNumberOfArgumentsException("getBettor", args.length, 0);
            }
        } else {
            if (transName.equals("getBetPrediction")) {
                if (args.length == 0) {
                    BetPrediction returnObj = this.getBetPrediction(__st);
                    __st.mapReturnedObject(returnObj, false);
                    Builder returnWrapperBuilder = InterfaceImplementerWrapper.newBuilder();
                    returnWrapperBuilder.setGuid(returnObj.__getGUID());
                    returnWrapperBuilder.setClassName(returnObj.getClass().getName());
                    returnBytes = Base64 .getEncoder().encode(returnWrapperBuilder.build().toByteArray());
                } else {
                    System.err.println("Wrong number of arguments in invocation.");
                    throw new WrongNumberOfArgumentsException("getBetPrediction", args.length, 0);
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
        org.hyperledger.fabric.example.BettingOuterClass.BetOrGUID.Builder builder = org.hyperledger.fabric.example.BettingOuterClass.BetOrGUID.newBuilder();
        builder.setObj(this.archive());
        BetOrGUID wrappedObject = builder.build();
        return wrappedObject.toByteArray();
    }

    public org.hyperledger.fabric.example.BettingOuterClass.Bet archive()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.BettingOuterClass.Bet.Builder builder = org.hyperledger.fabric.example.BettingOuterClass.Bet.newBuilder();
        builder.setGuid(__guid);
        if (bettor!= null) {
            String bettorArchiveID = bettor.__getGUID();
            builder.setBettor(bettorArchiveID);
        }
        if (prediction!= null) {
            String predictionArchiveID = prediction.__getGUID();
            builder.setPrediction(predictionArchiveID);
        }
        return builder.build();
    }

    public void initFromArchive(Object archiveIn, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.BettingOuterClass.Bet archive = ((org.hyperledger.fabric.example.BettingOuterClass.Bet) archiveIn);
        __guid = archive.getGuid();
        if (__bettorIsInScope()) {
            String bettorID = archive.getBettor();
            Bettor bettorVal = ((Bettor) __st.getEntry(bettorID));
            if (bettorVal!= null) {
                bettor = bettorVal;
            } else {
                bettor = new Bettor(bettorID);
                __st.putEntry(bettorID, bettor);
            }
        }
        if (__predictionIsInScope()) {
            String predictionID = archive.getPrediction();
            BetPrediction predictionVal = ((BetPrediction) __st.getEntry(predictionID));
            if (predictionVal!= null) {
                prediction = predictionVal;
            } else {
                prediction = new BetPrediction(predictionID);
                __st.putEntry(predictionID, prediction);
            }
        }
        __loaded = true;
    }

    public Bet __initFromArchiveBytes(byte[] archiveBytes, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.BettingOuterClass.Bet archive = org.hyperledger.fabric.example.BettingOuterClass.Bet.parseFrom(archiveBytes);
        initFromArchive(archive, __st);
        __loaded = true;
        return this;
    }

    @Override
    public byte[] init(SerializationState __st, byte[][] args)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        if (args.length!= 2) {
            throw new com.google.protobuf.InvalidProtocolBufferException("Incorrect number of arguments to constructor.");
        }
        byte[] decoded0 = Base64 .getDecoder().decode(args[ 0 ]);
        BettorOrGUID archive0 = BettorOrGUID.parseFrom(decoded0);
        Bettor unarchivedObj0;
        if (archive0 .getEitherCase().equals((org.hyperledger.fabric.example.BettingOuterClass.BettorOrGUID.EitherCase.GUID))) {
            unarchivedObj0 = ((Bettor) __st.loadContractWithGUID(__st.getStub(), archive0 .getGuid(), false, false));
        } else {
            unarchivedObj0 = new Bettor();
            unarchivedObj0 .initFromArchive(archive0 .getObj(), __st);
        }
        byte[] decoded1 = Base64 .getDecoder().decode(args[ 1 ]);
        BetPredictionOrGUID archive1 = BetPredictionOrGUID.parseFrom(decoded1);
        BetPrediction unarchivedObj1;
        if (archive1 .getEitherCase().equals((org.hyperledger.fabric.example.BettingOuterClass.BetPredictionOrGUID.EitherCase.GUID))) {
            unarchivedObj1 = ((BetPrediction) __st.loadContractWithGUID(__st.getStub(), archive1 .getGuid(), false, false));
        } else {
            unarchivedObj1 = new BetPrediction();
            unarchivedObj1 .initFromArchive(archive1 .getObj(), __st);
        }
        new_Bet(unarchivedObj0, unarchivedObj1, __st);
        return new byte[ 0 ] ;
    }
}
