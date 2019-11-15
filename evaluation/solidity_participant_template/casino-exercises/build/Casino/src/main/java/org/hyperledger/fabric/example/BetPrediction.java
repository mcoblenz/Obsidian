package org.hyperledger.fabric.example;

import java.nio.charset.StandardCharsets;
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
import org.hyperledger.fabric.example.BettingOuterClass.BetPredictionOrGUID;

public class BetPrediction
    implements ObsidianSerialized
{
    private String __guid;
    private boolean __modified;
    private boolean __loaded;
    private boolean __constructorReturnsOwned = false;
    public String predictedOutcome = "";
    public java.math.BigInteger betAmount = java.math.BigInteger.valueOf(0);
    static HashSet<String> transactionsWithOwnedReceiversAtBeginning;
    static HashSet<String> transactionsWithOwnedReceiversAtEnd;
    public boolean __isInsideInvocation = false;

    public BetPrediction(String __guid_) {
        __modified = false;
        __loaded = false;
        __guid = __guid_;
    }

    public BetPrediction(String predOut, java.math.BigInteger amount, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        new_BetPrediction(predOut, amount, __st);
    }

    public BetPrediction(SerializationState __st)
        throws ObsidianRevertException
    {
        new_BetPrediction(__st);
    }

    public BetPrediction() {
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

    public boolean __predictedOutcomeIsInScope()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        return true;
    }

    public boolean __betAmountIsInScope()
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

    private void new_BetPrediction(String predOut, java.math.BigInteger amount, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __constructorReturnsOwned = true;
        this.predictedOutcome = predOut;
        __modified = true;
        this.betAmount = amount;
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

    public String getPredOutcome(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/betting-exercises/betting_utils.obs", 0);
            } else {
                __isInsideInvocation = true;
                return this.predictedOutcome;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    public java.math.BigInteger getBetAmount(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/betting-exercises/betting_utils.obs", 0);
            } else {
                __isInsideInvocation = true;
                return this.betAmount;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    private void new_BetPrediction(SerializationState __st) {
        __guid = __st.getUUIDFactory().newUUID().toString();
        predictedOutcome = "";
        betAmount = java.math.BigInteger.valueOf(0);
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
        if (transName.equals("getBetAmount")) {
            if (args.length == 0) {
                java.math.BigInteger returnObj = this.getBetAmount(__st);
                returnBytes = returnObj.toString().getBytes(StandardCharsets.UTF_8);
            } else {
                System.err.println("Wrong number of arguments in invocation.");
                throw new WrongNumberOfArgumentsException("getBetAmount", args.length, 0);
            }
        } else {
            if (transName.equals("getPredOutcome")) {
                if (args.length == 0) {
                    String returnObj = this.getPredOutcome(__st);
                    returnBytes = returnObj.getBytes(StandardCharsets.UTF_8);
                } else {
                    System.err.println("Wrong number of arguments in invocation.");
                    throw new WrongNumberOfArgumentsException("getPredOutcome", args.length, 0);
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
        org.hyperledger.fabric.example.BettingOuterClass.BetPredictionOrGUID.Builder builder = org.hyperledger.fabric.example.BettingOuterClass.BetPredictionOrGUID.newBuilder();
        builder.setObj(this.archive());
        BetPredictionOrGUID wrappedObject = builder.build();
        return wrappedObject.toByteArray();
    }

    public org.hyperledger.fabric.example.BettingOuterClass.BetPrediction archive()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.BettingOuterClass.BetPrediction.Builder builder = org.hyperledger.fabric.example.BettingOuterClass.BetPrediction.newBuilder();
        builder.setGuid(__guid);
        if (predictedOutcome!= null) {
            builder.setPredictedOutcome(predictedOutcome);
        }
        if (betAmount!= null) {
            builder.setBetAmount(ByteString.copyFrom(betAmount.toByteArray()));
        }
        return builder.build();
    }

    public void initFromArchive(Object archiveIn, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.BettingOuterClass.BetPrediction archive = ((org.hyperledger.fabric.example.BettingOuterClass.BetPrediction) archiveIn);
        __guid = archive.getGuid();
        if (__predictedOutcomeIsInScope()) {
            if (!archive.getPredictedOutcome().isEmpty()) {
                predictedOutcome = archive.getPredictedOutcome();
            }
        }
        if (__betAmountIsInScope()) {
            if (!archive.getBetAmount().isEmpty()) {
                betAmount = new java.math.BigInteger(archive.getBetAmount().toByteArray());
            }
        }
        __loaded = true;
    }

    public BetPrediction __initFromArchiveBytes(byte[] archiveBytes, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.BettingOuterClass.BetPrediction archive = org.hyperledger.fabric.example.BettingOuterClass.BetPrediction.parseFrom(archiveBytes);
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
        java.math.BigInteger unmarshalledInt1 = new java.math.BigInteger(new String(args[ 1 ], StandardCharsets.UTF_8));
        if (unmarshalledInt1 == null) {
            throw new BadArgumentException(new String(args[ 1 ], StandardCharsets.UTF_8));
        }
        new_BetPrediction(new String(args[ 0 ], StandardCharsets.UTF_8), unmarshalledInt1, __st);
        return new byte[ 0 ] ;
    }
}
