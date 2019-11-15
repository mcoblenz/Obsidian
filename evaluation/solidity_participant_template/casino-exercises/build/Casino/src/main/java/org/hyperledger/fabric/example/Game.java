package org.hyperledger.fabric.example;

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
import org.hyperledger.fabric.example.BettingOuterClass.GameOrGUID;

public class Game
    implements ObsidianSerialized
{
    private Game.State_Game __state;
    private Game.State_BeforePlay __stateBeforePlay;
    private Game.State_Playing __statePlaying;
    private Game.State_FinishedPlaying __stateFinishedPlaying;
    private String __guid;
    private boolean __modified;
    private boolean __loaded;
    private boolean __constructorReturnsOwned = false;
    static HashSet<String> transactionsWithOwnedReceiversAtBeginning;
    static HashSet<String> transactionsWithOwnedReceiversAtEnd;
    public boolean __isInsideInvocation = false;

    public Game(String __guid_) {
        __modified = false;
        __loaded = false;
        __guid = __guid_;
    }

    public Game(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        new_Game(__st);
    }

    public Game() {
        __modified = true;
        __loaded = false;
    }

    public Game.State_Game getState(SerializationState __st)
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
        if (this.getState(null) == Game.State_Game.BeforePlay) {
            __stateBeforePlay = null;
        }
        if (this.getState(null) == Game.State_Game.FinishedPlaying) {
            __stateFinishedPlaying = null;
        }
        if (this.getState(null) == Game.State_Game.Playing) {
            __statePlaying = null;
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
        if (this.__modified) {
            result.add(this);
        }
        __modified = false;
        return result;
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

    private void new_Game(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __constructorReturnsOwned = true;
        __oldStateToNull();
        __stateBeforePlay = new Game.State_BeforePlay();
        __state = Game.State_Game.BeforePlay;
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

    public void startPlaying(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        if (this.getState(__st)!= org.hyperledger.fabric.example.Game.State_Game.BeforePlay) {
            throw new InvalidStateException(this, this.getState(__st).toString(), "startPlaying");
        }
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/betting-exercises/betting_utils.obs", 0);
            } else {
                __isInsideInvocation = true;
                __oldStateToNull();
                __statePlaying = new Game.State_Playing();
                __state = Game.State_Game.Playing;
                __modified = true;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    public void finishPlaying(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        if (this.getState(__st)!= org.hyperledger.fabric.example.Game.State_Game.Playing) {
            throw new InvalidStateException(this, this.getState(__st).toString(), "finishPlaying");
        }
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/betting-exercises/betting_utils.obs", 0);
            } else {
                __isInsideInvocation = true;
                __oldStateToNull();
                __stateFinishedPlaying = new Game.State_FinishedPlaying();
                __state = Game.State_Game.FinishedPlaying;
                __modified = true;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    public String calculateOutcome(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        if (this.getState(__st)!= org.hyperledger.fabric.example.Game.State_Game.FinishedPlaying) {
            throw new InvalidStateException(this, this.getState(__st).toString(), "calculateOutcome");
        }
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/betting-exercises/betting_utils.obs", 0);
            } else {
                __isInsideInvocation = true;
                return "";
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    @Override
    public boolean methodReceiverIsOwnedAtBeginning(String methodName) {
        if (transactionsWithOwnedReceiversAtBeginning == null) {
            transactionsWithOwnedReceiversAtBeginning = new HashSet<String>();
            transactionsWithOwnedReceiversAtBeginning.add("startPlaying");
            transactionsWithOwnedReceiversAtBeginning.add("finishPlaying");
            transactionsWithOwnedReceiversAtBeginning.add("calculateOutcome");
        }
        return transactionsWithOwnedReceiversAtBeginning.contains(methodName);
    }

    @Override
    public boolean methodReceiverIsOwnedAtEnd(String methodName) {
        if (transactionsWithOwnedReceiversAtEnd == null) {
            transactionsWithOwnedReceiversAtEnd = new HashSet<String>();
            transactionsWithOwnedReceiversAtBeginning.add("startPlaying");
            transactionsWithOwnedReceiversAtBeginning.add("finishPlaying");
            transactionsWithOwnedReceiversAtBeginning.add("calculateOutcome");
        }
        return transactionsWithOwnedReceiversAtEnd.contains(methodName);
    }

    public byte[] run(SerializationState __st, String transName, byte[][] args)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, NoSuchTransactionException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        byte[] returnBytes = new byte[ 0 ] ;
        if (transName.equals("startPlaying")) {
            if (args.length == 0) {
                this.startPlaying(__st);
            } else {
                System.err.println("Wrong number of arguments in invocation.");
                throw new WrongNumberOfArgumentsException("startPlaying", args.length, 0);
            }
        } else {
            if (transName.equals("calculateOutcome")) {
                if (args.length == 0) {
                    String returnObj = this.calculateOutcome(__st);
                    returnBytes = returnObj.getBytes(StandardCharsets.UTF_8);
                } else {
                    System.err.println("Wrong number of arguments in invocation.");
                    throw new WrongNumberOfArgumentsException("calculateOutcome", args.length, 0);
                }
            } else {
                if (transName.equals("finishPlaying")) {
                    if (args.length == 0) {
                        this.finishPlaying(__st);
                    } else {
                        System.err.println("Wrong number of arguments in invocation.");
                        throw new WrongNumberOfArgumentsException("finishPlaying", args.length, 0);
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
        org.hyperledger.fabric.example.BettingOuterClass.GameOrGUID.Builder builder = org.hyperledger.fabric.example.BettingOuterClass.GameOrGUID.newBuilder();
        builder.setObj(this.archive());
        GameOrGUID wrappedObject = builder.build();
        return wrappedObject.toByteArray();
    }

    public org.hyperledger.fabric.example.BettingOuterClass.Game archive()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.BettingOuterClass.Game.Builder builder = org.hyperledger.fabric.example.BettingOuterClass.Game.newBuilder();
        builder.setGuid(__guid);
        if (Game.State_Game.BeforePlay == this.getState(null)) {
            __stateBeforePlay.archive();
            builder.setStateBeforePlay(__stateBeforePlay.archive());
        }
        if (Game.State_Game.Playing == this.getState(null)) {
            __statePlaying.archive();
            builder.setStatePlaying(__statePlaying.archive());
        }
        if (Game.State_Game.FinishedPlaying == this.getState(null)) {
            __stateFinishedPlaying.archive();
            builder.setStateFinishedPlaying(__stateFinishedPlaying.archive());
        }
        return builder.build();
    }

    public void initFromArchive(Object archiveIn, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.BettingOuterClass.Game archive = ((org.hyperledger.fabric.example.BettingOuterClass.Game) archiveIn);
        __guid = archive.getGuid();
        if (archive.getStateCase().equals((org.hyperledger.fabric.example.BettingOuterClass.Game.StateCase.STATEBEFOREPLAY))) {
            __state = Game.State_Game.BeforePlay;
        }
        if (archive.getStateCase().equals((org.hyperledger.fabric.example.BettingOuterClass.Game.StateCase.STATEPLAYING))) {
            __state = Game.State_Game.Playing;
        }
        if (archive.getStateCase().equals((org.hyperledger.fabric.example.BettingOuterClass.Game.StateCase.STATEFINISHEDPLAYING))) {
            __state = Game.State_Game.FinishedPlaying;
        }
        __loaded = true;
    }

    public Game __initFromArchiveBytes(byte[] archiveBytes, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.BettingOuterClass.Game archive = org.hyperledger.fabric.example.BettingOuterClass.Game.parseFrom(archiveBytes);
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
        new_Game(__st);
        return new byte[ 0 ] ;
    }

    public class State_BeforePlay {

        public void initFromArchive(org.hyperledger.fabric.example.BettingOuterClass.Game.BeforePlay archive) {
        }

        public Game.State_BeforePlay __initFromArchiveBytes(byte[] archiveBytes)
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.BettingOuterClass.Game.BeforePlay archive = org.hyperledger.fabric.example.BettingOuterClass.Game.BeforePlay.parseFrom(archiveBytes);
            initFromArchive(archive);
            return this;
        }

        public org.hyperledger.fabric.example.BettingOuterClass.Game.BeforePlay archive()
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.BettingOuterClass.Game.BeforePlay.Builder builder = org.hyperledger.fabric.example.BettingOuterClass.Game.BeforePlay.newBuilder();
            return builder.build();
        }
    }

    public class State_FinishedPlaying {

        public void initFromArchive(org.hyperledger.fabric.example.BettingOuterClass.Game.FinishedPlaying archive) {
        }

        public Game.State_FinishedPlaying __initFromArchiveBytes(byte[] archiveBytes)
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.BettingOuterClass.Game.FinishedPlaying archive = org.hyperledger.fabric.example.BettingOuterClass.Game.FinishedPlaying.parseFrom(archiveBytes);
            initFromArchive(archive);
            return this;
        }

        public org.hyperledger.fabric.example.BettingOuterClass.Game.FinishedPlaying archive()
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.BettingOuterClass.Game.FinishedPlaying.Builder builder = org.hyperledger.fabric.example.BettingOuterClass.Game.FinishedPlaying.newBuilder();
            return builder.build();
        }
    }

    public enum State_Game {
        BeforePlay,
        Playing,
        FinishedPlaying;
    }

    public class State_Playing {

        public void initFromArchive(org.hyperledger.fabric.example.BettingOuterClass.Game.Playing archive) {
        }

        public Game.State_Playing __initFromArchiveBytes(byte[] archiveBytes)
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.BettingOuterClass.Game.Playing archive = org.hyperledger.fabric.example.BettingOuterClass.Game.Playing.parseFrom(archiveBytes);
            initFromArchive(archive);
            return this;
        }

        public org.hyperledger.fabric.example.BettingOuterClass.Game.Playing archive()
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.BettingOuterClass.Game.Playing.Builder builder = org.hyperledger.fabric.example.BettingOuterClass.Game.Playing.newBuilder();
            return builder.build();
        }
    }
}
