package org.hyperledger.fabric.example;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.HashSet;
import java.util.Set;
import edu.cmu.cs.obsidian.chaincode.BadArgumentException;
import edu.cmu.cs.obsidian.chaincode.BadTransactionException;
import edu.cmu.cs.obsidian.chaincode.HyperledgerChaincodeBase;
import edu.cmu.cs.obsidian.chaincode.IllegalOwnershipConsumptionException;
import edu.cmu.cs.obsidian.chaincode.InvalidStateException;
import edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException;
import edu.cmu.cs.obsidian.chaincode.ObsidianRevertException;
import edu.cmu.cs.obsidian.chaincode.ObsidianSerialized;
import edu.cmu.cs.obsidian.chaincode.ReentrancyException;
import edu.cmu.cs.obsidian.chaincode.SerializationState;
import edu.cmu.cs.obsidian.chaincode.StateLockException;
import edu.cmu.cs.obsidian.chaincode.WrongNumberOfArgumentsException;
import org.hyperledger.fabric.example.Auction-solnativeOuterClass.AuctionOrGUID;
import org.hyperledger.fabric.example.Auction-solnativeOuterClass.MoneyOrGUID;
import org.hyperledger.fabric.example.Auction-solnativeOuterClass.ParticipantOrGUID;

public class Auction
    extends HyperledgerChaincodeBase
    implements ObsidianSerialized
{
    private State_Auction __state;
    private Auction.State_Open __stateOpen;
    private Auction.State_BidsMade __stateBidsMade;
    private Auction.State_Closed __stateClosed;
    private String __guid;
    private boolean __modified;
    private boolean __loaded;
    private boolean __constructorReturnsOwned = false;
    public Participant seller;
    public Money maxBid;
    public Participant maxBidder;
    static HashSet<String> transactionsWithOwnedReceiversAtBeginning;
    static HashSet<String> transactionsWithOwnedReceiversAtEnd;
    public boolean __isInsideInvocation = false;

    public Auction(String __guid_) {
        __modified = false;
        __loaded = false;
        __guid = __guid_;
    }

    public Auction(Participant s, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        new_Auction(s, __st);
    }

    public Auction(SerializationState __st)
        throws ObsidianRevertException
    {
        new_Auction(__st);
    }

    public Auction() {
        __modified = true;
        __loaded = false;
    }

    public State_Auction getState(SerializationState __st)
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
        if (this.getState(null) == State_Auction.BidsMade) {
            __stateBidsMade = null;
        }
        if (this.getState(null) == State_Auction.Closed) {
            __stateClosed = null;
        }
        if (this.getState(null) == State_Auction.Open) {
            __stateOpen = null;
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
        if (__sellerIsInScope()) {
            if (!checked.contains(seller)) {
                result.addAll(seller.__resetModified(checked));
            }
        }
        if (__maxBidIsInScope()) {
            if (!checked.contains(maxBid)) {
                result.addAll(maxBid.__resetModified(checked));
            }
        }
        if (__maxBidderIsInScope()) {
            if (!checked.contains(maxBidder)) {
                result.addAll(maxBidder.__resetModified(checked));
            }
        }
        if (this.__modified) {
            result.add(this);
        }
        __modified = false;
        return result;
    }

    public boolean __sellerIsInScope()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        return true;
    }

    public boolean __maxBidIsInScope()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        return java.util.Arrays.asList(new String[] {"BidsMade"}).contains(this.getState(null).toString());
    }

    public boolean __maxBidderIsInScope()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        return java.util.Arrays.asList(new String[] {"BidsMade"}).contains(this.getState(null).toString());
    }

    public void __restoreObject(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        __guid = "Auction";
        if (!__loaded) {
            String __archive_string = __st.getStub().getStringState(__guid);
            byte[] __archive_bytes = __archive_string.getBytes();
            __initFromArchiveBytes(__archive_bytes, __st);
            __loaded = true;
        }
    }

    protected void __unload() {
        __loaded = false;
    }

    private void new_Auction(Participant s, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __constructorReturnsOwned = true;
        __oldStateToNull();
        __stateOpen = new Auction.State_Open();
        this.seller = s;
        __modified = true;
        __state = State_Auction.Open;
        __modified = true;
        __guid = "Auction";
        __modified = true;
        __loaded = true;
        __st.putEntry(__guid, this);
        __st.mapReturnedObject(this, false);
    }

    @Override
    public boolean constructorReturnsOwnedReference() {
        return __constructorReturnsOwned;
    }

    public void bid(Money money, Participant bidder, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/solidity_participant_template/auction-exercises/auction-solnative.obs", 0);
            } else {
                __isInsideInvocation = true;
                if (new HashSet<String>(java.util.Arrays.asList("Open")).contains(this.getState(__st).toString())) {
                    __oldStateToNull();
                    __stateBidsMade = new Auction.State_BidsMade();
                    this.maxBidder = bidder;
                    __modified = true;
                    this.maxBid = money;
                    __modified = true;
                    __state = State_Auction.BidsMade;
                    __modified = true;
                } else {
                    if (new HashSet<String>(java.util.Arrays.asList("BidsMade")).contains(this.getState(__st).toString())) {
                        if (money.getAmount(__st).compareTo(this.maxBid.getAmount(__st)) == 1) {
                            this.maxBidder.receivePayment(this.maxBid, __st);
                            this.maxBid = money;
                            __modified = true;
                        } else {
                            bidder.receivePayment(money, __st);
                        }
                    } else {
                        throw new ObsidianRevertException("/Users/mcoblenz/code/Obsidian/evaluation/solidity_participant_template/auction-exercises/auction-solnative.obs", 52, "Can only make a bid on an open auction.");
                    }
                }
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    public void finishBidding(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/solidity_participant_template/auction-exercises/auction-solnative.obs", 0);
            } else {
                __isInsideInvocation = true;
                if (new HashSet<String>(java.util.Arrays.asList("BidsMade")).contains(this.getState(__st).toString())) {
                    this.seller.receivePayment(this.maxBid, __st);
                    __oldStateToNull();
                    __stateClosed = new Auction.State_Closed();
                    __state = State_Auction.Closed;
                    __modified = true;
                } else {
                    throw new ObsidianRevertException("/Users/mcoblenz/code/Obsidian/evaluation/solidity_participant_template/auction-exercises/auction-solnative.obs", 64, "Can only finishBidding in state BidsMade");
                }
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    private void new_Auction(SerializationState __st) {
        __guid = "Auction";
        seller = null;
        maxBid = null;
        maxBidder = null;
        __st.flushEntries();
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

    public byte[] query(SerializationState __st, String transName, byte[][] args) {
        return new byte[ 0 ] ;
    }

    public byte[] getChaincodeID() {
        return new byte[ 0 ] ;
    }

    public static void main(String[] args) {
        Auction instance = new Auction("Auction");
        instance.delegatedMain(args);
    }

    public byte[] run(SerializationState __st, String transName, byte[][] args)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, NoSuchTransactionException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        __st.mapReturnedObject(this, false);
        byte[] returnBytes = new byte[ 0 ] ;
        if (transName.equals("finishBidding")) {
            if (args.length == 0) {
                this.finishBidding(__st);
            } else {
                System.err.println("Wrong number of arguments in invocation.");
                throw new WrongNumberOfArgumentsException("finishBidding", args.length, 0);
            }
        } else {
            if (transName.equals("bid")) {
                if (args.length == 2) {
                    byte[] decoded0 = Base64 .getDecoder().decode(args[ 0 ]);
                    MoneyOrGUID archive0 = MoneyOrGUID.parseFrom(decoded0);
                    Money unarchivedObj0;
                    if (archive0 .getEitherCase().equals((org.hyperledger.fabric.example.Auction-solnativeOuterClass.MoneyOrGUID.EitherCase.GUID))) {
                        unarchivedObj0 = ((Money) __st.loadContractWithGUID(__st.getStub(), archive0 .getGuid(), true, false));
                    } else {
                        unarchivedObj0 = new Money();
                        unarchivedObj0 .initFromArchive(archive0 .getObj(), __st);
                    }
                    Money money = unarchivedObj0;
                    byte[] decoded1 = Base64 .getDecoder().decode(args[ 1 ]);
                    ParticipantOrGUID archive1 = ParticipantOrGUID.parseFrom(decoded1);
                    Participant unarchivedObj1;
                    if (archive1 .getEitherCase().equals((org.hyperledger.fabric.example.Auction-solnativeOuterClass.ParticipantOrGUID.EitherCase.GUID))) {
                        unarchivedObj1 = ((Participant) __st.loadContractWithGUID(__st.getStub(), archive1 .getGuid(), false, false));
                    } else {
                        unarchivedObj1 = new Participant();
                        unarchivedObj1 .initFromArchive(archive1 .getObj(), __st);
                    }
                    Participant bidder = unarchivedObj1;
                    this.bid(money, bidder, __st);
                } else {
                    System.err.println("Wrong number of arguments in invocation.");
                    throw new WrongNumberOfArgumentsException("bid", args.length, 2);
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
        org.hyperledger.fabric.example.Auction-solnativeOuterClass.AuctionOrGUID.Builder builder = org.hyperledger.fabric.example.Auction-solnativeOuterClass.AuctionOrGUID.newBuilder();
        builder.setObj(this.archive());
        AuctionOrGUID wrappedObject = builder.build();
        return wrappedObject.toByteArray();
    }

    public org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction archive()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.Builder builder = org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.newBuilder();
        builder.setGuid(__guid);
        if (seller!= null) {
            String sellerArchiveID = seller.__getGUID();
            builder.setSeller(sellerArchiveID);
        }
        if (maxBid!= null) {
            String maxBidArchiveID = maxBid.__getGUID();
            builder.setMaxBid(maxBidArchiveID);
        }
        if (maxBidder!= null) {
            String maxBidderArchiveID = maxBidder.__getGUID();
            builder.setMaxBidder(maxBidderArchiveID);
        }
        if (State_Auction.Open == this.getState(null)) {
            __stateOpen.archive();
            builder.setStateOpen(__stateOpen.archive());
        }
        if (State_Auction.BidsMade == this.getState(null)) {
            __stateBidsMade.archive();
            builder.setStateBidsMade(__stateBidsMade.archive());
        }
        if (State_Auction.Closed == this.getState(null)) {
            __stateClosed.archive();
            builder.setStateClosed(__stateClosed.archive());
        }
        return builder.build();
    }

    public void initFromArchive(Object archiveIn, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction archive = ((org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction) archiveIn);
        __guid = archive.getGuid();
        if (archive.getStateCase().equals((org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.StateCase.STATEOPEN))) {
            __state = State_Auction.Open;
        }
        if (archive.getStateCase().equals((org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.StateCase.STATEBIDSMADE))) {
            __state = State_Auction.BidsMade;
        }
        if (archive.getStateCase().equals((org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.StateCase.STATECLOSED))) {
            __state = State_Auction.Closed;
        }
        if (__sellerIsInScope()) {
            String sellerID = archive.getSeller();
            Participant sellerVal = ((Participant) __st.getEntry(sellerID));
            if (sellerVal!= null) {
                seller = sellerVal;
            } else {
                seller = new Participant(sellerID);
                __st.putEntry(sellerID, seller);
            }
        }
        if (__maxBidIsInScope()) {
            String maxBidID = archive.getMaxBid();
            Money maxBidVal = ((Money) __st.getEntry(maxBidID));
            if (maxBidVal!= null) {
                maxBid = maxBidVal;
            } else {
                maxBid = new Money(maxBidID);
                __st.putEntry(maxBidID, maxBid);
            }
        }
        if (__maxBidderIsInScope()) {
            String maxBidderID = archive.getMaxBidder();
            Participant maxBidderVal = ((Participant) __st.getEntry(maxBidderID));
            if (maxBidderVal!= null) {
                maxBidder = maxBidderVal;
            } else {
                maxBidder = new Participant(maxBidderID);
                __st.putEntry(maxBidderID, maxBidder);
            }
        }
        __loaded = true;
    }

    public Auction __initFromArchiveBytes(byte[] archiveBytes, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction archive = org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.parseFrom(archiveBytes);
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
        ParticipantOrGUID archive0 = ParticipantOrGUID.parseFrom(decoded0);
        Participant unarchivedObj0;
        if (archive0 .getEitherCase().equals((org.hyperledger.fabric.example.Auction-solnativeOuterClass.ParticipantOrGUID.EitherCase.GUID))) {
            unarchivedObj0 = ((Participant) __st.loadContractWithGUID(__st.getStub(), archive0 .getGuid(), false, false));
        } else {
            unarchivedObj0 = new Participant();
            unarchivedObj0 .initFromArchive(archive0 .getObj(), __st);
        }
        new_Auction(unarchivedObj0, __st);
        return new byte[ 0 ] ;
    }

    public enum State_Auction {
        Open,
        BidsMade,
        Closed;
    }

    public class State_BidsMade {

        public void initFromArchive(org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.BidsMade archive) {
        }

        public Auction.State_BidsMade __initFromArchiveBytes(byte[] archiveBytes)
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.BidsMade archive = org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.BidsMade.parseFrom(archiveBytes);
            initFromArchive(archive);
            return this;
        }

        public org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.BidsMade archive()
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.BidsMade.Builder builder = org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.BidsMade.newBuilder();
            return builder.build();
        }
    }

    public class State_Closed {

        public void initFromArchive(org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.Closed archive) {
        }

        public Auction.State_Closed __initFromArchiveBytes(byte[] archiveBytes)
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.Closed archive = org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.Closed.parseFrom(archiveBytes);
            initFromArchive(archive);
            return this;
        }

        public org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.Closed archive()
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.Closed.Builder builder = org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.Closed.newBuilder();
            return builder.build();
        }
    }

    public class State_Open {

        public void initFromArchive(org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.Open archive) {
        }

        public Auction.State_Open __initFromArchiveBytes(byte[] archiveBytes)
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.Open archive = org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.Open.parseFrom(archiveBytes);
            initFromArchive(archive);
            return this;
        }

        public org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.Open archive()
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.Open.Builder builder = org.hyperledger.fabric.example.Auction-solnativeOuterClass.Auction.Open.newBuilder();
            return builder.build();
        }
    }
}
