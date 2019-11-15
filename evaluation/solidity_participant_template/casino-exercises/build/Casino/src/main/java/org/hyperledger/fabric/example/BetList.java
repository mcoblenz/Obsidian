package org.hyperledger.fabric.example;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.HashSet;
import java.util.Set;
import edu.cmu.cs.obsidian.chaincode.BadArgumentException;
import edu.cmu.cs.obsidian.chaincode.BadTransactionException;
import edu.cmu.cs.obsidian.chaincode.ChaincodeUtils;
import edu.cmu.cs.obsidian.chaincode.IllegalOwnershipConsumptionException;
import edu.cmu.cs.obsidian.chaincode.InvalidStateException;
import edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException;
import edu.cmu.cs.obsidian.chaincode.ObsidianRevertException;
import edu.cmu.cs.obsidian.chaincode.ObsidianSerialized;
import edu.cmu.cs.obsidian.chaincode.ReentrancyException;
import edu.cmu.cs.obsidian.chaincode.SerializationState;
import edu.cmu.cs.obsidian.chaincode.StateLockException;
import edu.cmu.cs.obsidian.chaincode.WrongNumberOfArgumentsException;
import org.hyperledger.fabric.example.BettingOuterClass.BetListOrGUID;
import org.hyperledger.fabric.example.BettingOuterClass.BetOrGUID;
import org.hyperledger.fabric.example.BettingOuterClass.BettorOrGUID;
import org.hyperledger.fabric.example.InterfaceImplementerWrapperOuterClass.InterfaceImplementerWrapper;
import org.hyperledger.fabric.example.InterfaceImplementerWrapperOuterClass.InterfaceImplementerWrapper.Builder;

public class BetList
    implements ObsidianSerialized
{
    private BetList.State_BetList __state;
    private BetList.State_Empty __stateEmpty;
    private BetList.State_HasItems __stateHasItems;
    private String __guid;
    private boolean __modified;
    private boolean __loaded;
    private boolean __constructorReturnsOwned = false;
    public Bet b;
    public BetList next;
    static HashSet<String> transactionsWithOwnedReceiversAtBeginning;
    static HashSet<String> transactionsWithOwnedReceiversAtEnd;
    public boolean __isInsideInvocation = false;

    public BetList(String __guid_) {
        __modified = false;
        __loaded = false;
        __guid = __guid_;
    }

    public BetList(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        new_BetList(__st);
    }

    public BetList() {
        __modified = true;
        __loaded = false;
    }

    public BetList.State_BetList getState(SerializationState __st)
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
        if (this.getState(null) == BetList.State_BetList.Empty) {
            __stateEmpty = null;
        }
        if (this.getState(null) == BetList.State_BetList.HasItems) {
            __stateHasItems = null;
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
        if (__bIsInScope()) {
            if (!checked.contains(b)) {
                result.addAll(b.__resetModified(checked));
            }
        }
        if (__nextIsInScope()) {
            if (!checked.contains(next)) {
                result.addAll(next.__resetModified(checked));
            }
        }
        if (this.__modified) {
            result.add(this);
        }
        __modified = false;
        return result;
    }

    public boolean __bIsInScope()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        return java.util.Arrays.asList(new String[] {"HasItems"}).contains(this.getState(null).toString());
    }

    public boolean __nextIsInScope()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        return java.util.Arrays.asList(new String[] {"HasItems"}).contains(this.getState(null).toString());
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

    private void new_BetList(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __constructorReturnsOwned = false;
        __oldStateToNull();
        __stateEmpty = new BetList.State_Empty();
        __state = BetList.State_BetList.Empty;
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

    public BetList getNext(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        if (this.getState(__st)!= org.hyperledger.fabric.example.BetList.State_BetList.HasItems) {
            throw new InvalidStateException(this, this.getState(__st).toString(), "getNext");
        }
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/betting-exercises/betting_utils.obs", 0);
            } else {
                __isInsideInvocation = true;
                return this.next;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    public Bet getValue(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        if (this.getState(__st)!= org.hyperledger.fabric.example.BetList.State_BetList.HasItems) {
            throw new InvalidStateException(this, this.getState(__st).toString(), "getValue");
        }
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/betting-exercises/betting_utils.obs", 0);
            } else {
                __isInsideInvocation = true;
                return this.b;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    public void append(Bet newB, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/betting-exercises/betting_utils.obs", 0);
            } else {
                __isInsideInvocation = true;
                if (new HashSet<String>(java.util.Arrays.asList("Empty")).contains(this.getState(__st).toString())) {
                    BetList bList = new BetList(__st);
                    __oldStateToNull();
                    __stateHasItems = new BetList.State_HasItems();
                    this.b = newB;
                    __modified = true;
                    this.next = bList;
                    __modified = true;
                    __state = BetList.State_BetList.HasItems;
                    __modified = true;
                } else {
                    this.next.append(newB, __st);
                }
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    public Bet popSpecific(Bettor bettor, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/betting-exercises/betting_utils.obs", 0);
            } else {
                __isInsideInvocation = true;
                if (new HashSet<String>(java.util.Arrays.asList("HasItems")).contains(this.getState(__st).toString())) {
                    if (this.b.getBettor(__st).equals(bettor)) {
                        Bet temp = this.b;
                        if (new HashSet<String>(java.util.Arrays.asList("Empty")).contains(this.next.getState(__st).toString())) {
                            try {
                                __st.beginStateLock(this.next);
                                __oldStateToNull();
                                __stateEmpty = new BetList.State_Empty();
                                __state = BetList.State_BetList.Empty;
                                __modified = true;
                            } finally {
                                __st.endStateLock(this.next);
                            }
                        } else {
                            Bet newB = this.next.getValue(__st);
                            BetList bList = this.next.getNext(__st);
                            __oldStateToNull();
                            __stateHasItems = new BetList.State_HasItems();
                            this.b = newB;
                            __modified = true;
                            this.next = bList;
                            __modified = true;
                            __state = BetList.State_BetList.HasItems;
                            __modified = true;
                        }
                        return temp;
                    } else {
                        return this.next.popSpecific(bettor, __st);
                    }
                } else {
                    throw new ObsidianRevertException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/betting-exercises/betting_utils.obs", 154, "No bets in list");
                }
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    public boolean contains(Bettor bettor, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/betting-exercises/betting_utils.obs", 0);
            } else {
                __isInsideInvocation = true;
                if (new HashSet<String>(java.util.Arrays.asList("HasItems")).contains(this.getState(__st).toString())) {
                    if (this.b.getBettor(__st).equals(bettor)) {
                        return true;
                    } else {
                        return this.next.contains(bettor, __st);
                    }
                } else {
                    return false;
                }
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    @Override
    public boolean methodReceiverIsOwnedAtBeginning(String methodName) {
        if (transactionsWithOwnedReceiversAtBeginning == null) {
            transactionsWithOwnedReceiversAtBeginning = new HashSet<String>();
            transactionsWithOwnedReceiversAtBeginning.add("getNext");
            transactionsWithOwnedReceiversAtBeginning.add("getValue");
        }
        return transactionsWithOwnedReceiversAtBeginning.contains(methodName);
    }

    @Override
    public boolean methodReceiverIsOwnedAtEnd(String methodName) {
        if (transactionsWithOwnedReceiversAtEnd == null) {
            transactionsWithOwnedReceiversAtEnd = new HashSet<String>();
            transactionsWithOwnedReceiversAtBeginning.add("getNext");
            transactionsWithOwnedReceiversAtBeginning.add("getValue");
        }
        return transactionsWithOwnedReceiversAtEnd.contains(methodName);
    }

    public byte[] run(SerializationState __st, String transName, byte[][] args)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, NoSuchTransactionException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        byte[] returnBytes = new byte[ 0 ] ;
        if (transName.equals("getValue")) {
            if (args.length == 0) {
                Bet returnObj = this.getValue(__st);
                __st.mapReturnedObject(returnObj, false);
                Builder returnWrapperBuilder = InterfaceImplementerWrapper.newBuilder();
                returnWrapperBuilder.setGuid(returnObj.__getGUID());
                returnWrapperBuilder.setClassName(returnObj.getClass().getName());
                returnBytes = Base64 .getEncoder().encode(returnWrapperBuilder.build().toByteArray());
            } else {
                System.err.println("Wrong number of arguments in invocation.");
                throw new WrongNumberOfArgumentsException("getValue", args.length, 0);
            }
        } else {
            if (transName.equals("append")) {
                if (args.length == 1) {
                    byte[] decoded0 = Base64 .getDecoder().decode(args[ 0 ]);
                    BetOrGUID archive0 = BetOrGUID.parseFrom(decoded0);
                    Bet unarchivedObj0;
                    if (archive0 .getEitherCase().equals((org.hyperledger.fabric.example.BettingOuterClass.BetOrGUID.EitherCase.GUID))) {
                        unarchivedObj0 = ((Bet) __st.loadContractWithGUID(__st.getStub(), archive0 .getGuid(), false, false));
                    } else {
                        unarchivedObj0 = new Bet();
                        unarchivedObj0 .initFromArchive(archive0 .getObj(), __st);
                    }
                    Bet newB = unarchivedObj0;
                    this.append(newB, __st);
                } else {
                    System.err.println("Wrong number of arguments in invocation.");
                    throw new WrongNumberOfArgumentsException("append", args.length, 1);
                }
            } else {
                if (transName.equals("contains")) {
                    if (args.length == 1) {
                        byte[] decoded0 = Base64 .getDecoder().decode(args[ 0 ]);
                        BettorOrGUID archive0 = BettorOrGUID.parseFrom(decoded0);
                        Bettor unarchivedObj0;
                        if (archive0 .getEitherCase().equals((org.hyperledger.fabric.example.BettingOuterClass.BettorOrGUID.EitherCase.GUID))) {
                            unarchivedObj0 = ((Bettor) __st.loadContractWithGUID(__st.getStub(), archive0 .getGuid(), false, false));
                        } else {
                            unarchivedObj0 = new Bettor();
                            unarchivedObj0 .initFromArchive(archive0 .getObj(), __st);
                        }
                        Bettor bettor = unarchivedObj0;
                        boolean returnObj = this.contains(bettor, __st);
                        returnBytes = ChaincodeUtils.booleanToBytes(returnObj);
                    } else {
                        System.err.println("Wrong number of arguments in invocation.");
                        throw new WrongNumberOfArgumentsException("contains", args.length, 1);
                    }
                } else {
                    if (transName.equals("popSpecific")) {
                        if (args.length == 1) {
                            byte[] decoded0 = Base64 .getDecoder().decode(args[ 0 ]);
                            BettorOrGUID archive0 = BettorOrGUID.parseFrom(decoded0);
                            Bettor unarchivedObj0;
                            if (archive0 .getEitherCase().equals((org.hyperledger.fabric.example.BettingOuterClass.BettorOrGUID.EitherCase.GUID))) {
                                unarchivedObj0 = ((Bettor) __st.loadContractWithGUID(__st.getStub(), archive0 .getGuid(), false, false));
                            } else {
                                unarchivedObj0 = new Bettor();
                                unarchivedObj0 .initFromArchive(archive0 .getObj(), __st);
                            }
                            Bettor bettor = unarchivedObj0;
                            Bet returnObj = this.popSpecific(bettor, __st);
                            __st.mapReturnedObject(returnObj, false);
                            Builder returnWrapperBuilder = InterfaceImplementerWrapper.newBuilder();
                            returnWrapperBuilder.setGuid(returnObj.__getGUID());
                            returnWrapperBuilder.setClassName(returnObj.getClass().getName());
                            returnBytes = Base64 .getEncoder().encode(returnWrapperBuilder.build().toByteArray());
                        } else {
                            System.err.println("Wrong number of arguments in invocation.");
                            throw new WrongNumberOfArgumentsException("popSpecific", args.length, 1);
                        }
                    } else {
                        if (transName.equals("getNext")) {
                            if (args.length == 0) {
                                BetList returnObj = this.getNext(__st);
                                __st.mapReturnedObject(returnObj, false);
                                Builder returnWrapperBuilder = InterfaceImplementerWrapper.newBuilder();
                                returnWrapperBuilder.setGuid(returnObj.__getGUID());
                                returnWrapperBuilder.setClassName(returnObj.getClass().getName());
                                returnBytes = Base64 .getEncoder().encode(returnWrapperBuilder.build().toByteArray());
                            } else {
                                System.err.println("Wrong number of arguments in invocation.");
                                throw new WrongNumberOfArgumentsException("getNext", args.length, 0);
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
        org.hyperledger.fabric.example.BettingOuterClass.BetListOrGUID.Builder builder = org.hyperledger.fabric.example.BettingOuterClass.BetListOrGUID.newBuilder();
        builder.setObj(this.archive());
        BetListOrGUID wrappedObject = builder.build();
        return wrappedObject.toByteArray();
    }

    public org.hyperledger.fabric.example.BettingOuterClass.BetList archive()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.BettingOuterClass.BetList.Builder builder = org.hyperledger.fabric.example.BettingOuterClass.BetList.newBuilder();
        builder.setGuid(__guid);
        if (b!= null) {
            String bArchiveID = b.__getGUID();
            builder.setB(bArchiveID);
        }
        if (next!= null) {
            String nextArchiveID = next.__getGUID();
            builder.setNext(nextArchiveID);
        }
        if (BetList.State_BetList.Empty == this.getState(null)) {
            __stateEmpty.archive();
            builder.setStateEmpty(__stateEmpty.archive());
        }
        if (BetList.State_BetList.HasItems == this.getState(null)) {
            __stateHasItems.archive();
            builder.setStateHasItems(__stateHasItems.archive());
        }
        return builder.build();
    }

    public void initFromArchive(Object archiveIn, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.BettingOuterClass.BetList archive = ((org.hyperledger.fabric.example.BettingOuterClass.BetList) archiveIn);
        __guid = archive.getGuid();
        if (archive.getStateCase().equals((org.hyperledger.fabric.example.BettingOuterClass.BetList.StateCase.STATEEMPTY))) {
            __state = BetList.State_BetList.Empty;
        }
        if (archive.getStateCase().equals((org.hyperledger.fabric.example.BettingOuterClass.BetList.StateCase.STATEHASITEMS))) {
            __state = BetList.State_BetList.HasItems;
        }
        if (__bIsInScope()) {
            String bID = archive.getB();
            Bet bVal = ((Bet) __st.getEntry(bID));
            if (bVal!= null) {
                b = bVal;
            } else {
                b = new Bet(bID);
                __st.putEntry(bID, b);
            }
        }
        if (__nextIsInScope()) {
            String nextID = archive.getNext();
            BetList nextVal = ((BetList) __st.getEntry(nextID));
            if (nextVal!= null) {
                next = nextVal;
            } else {
                next = new BetList(nextID);
                __st.putEntry(nextID, next);
            }
        }
        __loaded = true;
    }

    public BetList __initFromArchiveBytes(byte[] archiveBytes, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.BettingOuterClass.BetList archive = org.hyperledger.fabric.example.BettingOuterClass.BetList.parseFrom(archiveBytes);
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
        new_BetList(__st);
        return new byte[ 0 ] ;
    }

    public enum State_BetList {
        Empty,
        HasItems;
    }

    public class State_Empty {

        public void initFromArchive(org.hyperledger.fabric.example.BettingOuterClass.BetList.Empty archive) {
        }

        public BetList.State_Empty __initFromArchiveBytes(byte[] archiveBytes)
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.BettingOuterClass.BetList.Empty archive = org.hyperledger.fabric.example.BettingOuterClass.BetList.Empty.parseFrom(archiveBytes);
            initFromArchive(archive);
            return this;
        }

        public org.hyperledger.fabric.example.BettingOuterClass.BetList.Empty archive()
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.BettingOuterClass.BetList.Empty.Builder builder = org.hyperledger.fabric.example.BettingOuterClass.BetList.Empty.newBuilder();
            return builder.build();
        }
    }

    public class State_HasItems {

        public void initFromArchive(org.hyperledger.fabric.example.BettingOuterClass.BetList.HasItems archive) {
        }

        public BetList.State_HasItems __initFromArchiveBytes(byte[] archiveBytes)
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.BettingOuterClass.BetList.HasItems archive = org.hyperledger.fabric.example.BettingOuterClass.BetList.HasItems.parseFrom(archiveBytes);
            initFromArchive(archive);
            return this;
        }

        public org.hyperledger.fabric.example.BettingOuterClass.BetList.HasItems archive()
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.BettingOuterClass.BetList.HasItems.Builder builder = org.hyperledger.fabric.example.BettingOuterClass.BetList.HasItems.newBuilder();
            return builder.build();
        }
    }
}
