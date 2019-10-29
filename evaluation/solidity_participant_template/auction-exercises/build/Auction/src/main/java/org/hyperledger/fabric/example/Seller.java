package org.hyperledger.fabric.example;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
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
import org.hyperledger.fabric.example.AuctionOuterClass.SellerOrGUID;
import org.hyperledger.fabric.example.InterfaceImplementerWrapperOuterClass.InterfaceImplementerWrapper;
import org.hyperledger.fabric.example.InterfaceImplementerWrapperOuterClass.InterfaceImplementerWrapper.Builder;

public class Seller
    implements ObsidianSerialized
{
    private Seller.State_Seller __state;
    private Seller.State_Sold __stateSold;
    private Seller.State_Unsold __stateUnsold;
    private Seller.State_InAuction __stateInAuction;
    private String __guid;
    private boolean __modified;
    private boolean __loaded;
    private boolean __constructorReturnsOwned = false;
    public Bid bid;
    public Item item;
    static HashSet<String> transactionsWithOwnedReceiversAtBeginning;
    static HashSet<String> transactionsWithOwnedReceiversAtEnd;
    public boolean __isInsideInvocation = false;

    public Seller(String __guid_) {
        __modified = false;
        __loaded = false;
        __guid = __guid_;
    }

    public Seller(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        new_Seller(__st);
    }

    public Seller() {
        __modified = true;
        __loaded = false;
    }

    public Seller.State_Seller getState(SerializationState __st)
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
        if (this.getState(null) == Seller.State_Seller.InAuction) {
            __stateInAuction = null;
        }
        if (this.getState(null) == Seller.State_Seller.Sold) {
            __stateSold = null;
        }
        if (this.getState(null) == Seller.State_Seller.Unsold) {
            __stateUnsold = null;
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
        if (__bidIsInScope()) {
            if (!checked.contains(bid)) {
                result.addAll(bid.__resetModified(checked));
            }
        }
        if (__itemIsInScope()) {
            if (!checked.contains(item)) {
                result.addAll(item.__resetModified(checked));
            }
        }
        if (this.__modified) {
            result.add(this);
        }
        __modified = false;
        return result;
    }

    public boolean __bidIsInScope()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        return Arrays.asList(new String[] {"Sold"}).contains(this.getState(null).toString());
    }

    public boolean __itemIsInScope()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        return Arrays.asList(new String[] {"Unsold"}).contains(this.getState(null).toString());
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

    private void new_Seller(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __constructorReturnsOwned = true;
        __oldStateToNull();
        __stateUnsold = new Seller.State_Unsold();
        this.item = new Item(__st);
        __modified = true;
        __state = Seller.State_Seller.Unsold;
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

    public Item giveItem(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        if (this.getState(__st)!= org.hyperledger.fabric.example.Seller.State_Seller.Unsold) {
            throw new InvalidStateException(this, this.getState(__st).toString(), "giveItem");
        }
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/auction-exercises/auction.obs", 0);
            } else {
                __isInsideInvocation = true;
                Item temp = this.item;
                __oldStateToNull();
                __stateInAuction = new Seller.State_InAuction();
                __state = Seller.State_Seller.InAuction;
                __modified = true;
                return temp;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    public void receiveBid(Bid b, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        if (this.getState(__st)!= org.hyperledger.fabric.example.Seller.State_Seller.Unsold) {
            throw new InvalidStateException(this, this.getState(__st).toString(), "receiveBid");
        }
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/auction-exercises/auction.obs", 0);
            } else {
                __isInsideInvocation = true;
                __oldStateToNull();
                __stateSold = new Seller.State_Sold();
                this.bid = b;
                __modified = true;
                __state = Seller.State_Seller.Sold;
                __modified = true;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    @Override
    public boolean methodReceiverIsOwnedAtBeginning(String methodName) {
        if (transactionsWithOwnedReceiversAtBeginning == null) {
            transactionsWithOwnedReceiversAtBeginning = new HashSet<String>();
            transactionsWithOwnedReceiversAtBeginning.add("giveItem");
            transactionsWithOwnedReceiversAtBeginning.add("receiveBid");
        }
        return transactionsWithOwnedReceiversAtBeginning.contains(methodName);
    }

    @Override
    public boolean methodReceiverIsOwnedAtEnd(String methodName) {
        if (transactionsWithOwnedReceiversAtEnd == null) {
            transactionsWithOwnedReceiversAtEnd = new HashSet<String>();
            transactionsWithOwnedReceiversAtBeginning.add("giveItem");
            transactionsWithOwnedReceiversAtBeginning.add("receiveBid");
        }
        return transactionsWithOwnedReceiversAtEnd.contains(methodName);
    }

    public byte[] run(SerializationState __st, String transName, byte[][] args)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, NoSuchTransactionException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        byte[] returnBytes = new byte[ 0 ] ;
        if (transName.equals("receiveBid")) {
            if (args.length == 1) {
                byte[] decoded0 = Base64 .getDecoder().decode(args[ 0 ]);
                BidOrGUID archive0 = BidOrGUID.parseFrom(decoded0);
                Bid unarchivedObj0;
                if (archive0 .getEitherCase().equals((org.hyperledger.fabric.example.AuctionOuterClass.BidOrGUID.EitherCase.GUID))) {
                    unarchivedObj0 = ((Bid) __st.loadContractWithGUID(__st.getStub(), archive0 .getGuid(), true, false));
                } else {
                    unarchivedObj0 = new Bid();
                    unarchivedObj0 .initFromArchive(archive0 .getObj(), __st);
                }
                Bid b = unarchivedObj0;
                this.receiveBid(b, __st);
            } else {
                System.err.println("Wrong number of arguments in invocation.");
                throw new WrongNumberOfArgumentsException("receiveBid", args.length, 1);
            }
        } else {
            if (transName.equals("giveItem")) {
                if (args.length == 0) {
                    Item returnObj = this.giveItem(__st);
                    __st.mapReturnedObject(returnObj, true);
                    Builder returnWrapperBuilder = InterfaceImplementerWrapper.newBuilder();
                    returnWrapperBuilder.setGuid(returnObj.__getGUID());
                    returnWrapperBuilder.setClassName(returnObj.getClass().getName());
                    returnBytes = Base64 .getEncoder().encode(returnWrapperBuilder.build().toByteArray());
                } else {
                    System.err.println("Wrong number of arguments in invocation.");
                    throw new WrongNumberOfArgumentsException("giveItem", args.length, 0);
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
        org.hyperledger.fabric.example.AuctionOuterClass.SellerOrGUID.Builder builder = org.hyperledger.fabric.example.AuctionOuterClass.SellerOrGUID.newBuilder();
        builder.setObj(this.archive());
        SellerOrGUID wrappedObject = builder.build();
        return wrappedObject.toByteArray();
    }

    public org.hyperledger.fabric.example.AuctionOuterClass.Seller archive()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.AuctionOuterClass.Seller.Builder builder = org.hyperledger.fabric.example.AuctionOuterClass.Seller.newBuilder();
        builder.setGuid(__guid);
        if (bid!= null) {
            String bidArchiveID = bid.__getGUID();
            builder.setBid(bidArchiveID);
        }
        if (item!= null) {
            String itemArchiveID = item.__getGUID();
            builder.setItem(itemArchiveID);
        }
        if (Seller.State_Seller.Sold == this.getState(null)) {
            __stateSold.archive();
            builder.setStateSold(__stateSold.archive());
        }
        if (Seller.State_Seller.Unsold == this.getState(null)) {
            __stateUnsold.archive();
            builder.setStateUnsold(__stateUnsold.archive());
        }
        if (Seller.State_Seller.InAuction == this.getState(null)) {
            __stateInAuction.archive();
            builder.setStateInAuction(__stateInAuction.archive());
        }
        return builder.build();
    }

    public void initFromArchive(Object archiveIn, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.AuctionOuterClass.Seller archive = ((org.hyperledger.fabric.example.AuctionOuterClass.Seller) archiveIn);
        __guid = archive.getGuid();
        if (archive.getStateCase().equals((org.hyperledger.fabric.example.AuctionOuterClass.Seller.StateCase.STATESOLD))) {
            __state = Seller.State_Seller.Sold;
        }
        if (archive.getStateCase().equals((org.hyperledger.fabric.example.AuctionOuterClass.Seller.StateCase.STATEUNSOLD))) {
            __state = Seller.State_Seller.Unsold;
        }
        if (archive.getStateCase().equals((org.hyperledger.fabric.example.AuctionOuterClass.Seller.StateCase.STATEINAUCTION))) {
            __state = Seller.State_Seller.InAuction;
        }
        if (__bidIsInScope()) {
            String bidID = archive.getBid();
            Bid bidVal = ((Bid) __st.getEntry(bidID));
            if (bidVal!= null) {
                bid = bidVal;
            } else {
                bid = new Bid(bidID);
                __st.putEntry(bidID, bid);
            }
        }
        if (__itemIsInScope()) {
            String itemID = archive.getItem();
            Item itemVal = ((Item) __st.getEntry(itemID));
            if (itemVal!= null) {
                item = itemVal;
            } else {
                item = new Item(itemID);
                __st.putEntry(itemID, item);
            }
        }
        __loaded = true;
    }

    public Seller __initFromArchiveBytes(byte[] archiveBytes, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.AuctionOuterClass.Seller archive = org.hyperledger.fabric.example.AuctionOuterClass.Seller.parseFrom(archiveBytes);
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
        new_Seller(__st);
        return new byte[ 0 ] ;
    }

    public class State_InAuction {

        public void initFromArchive(org.hyperledger.fabric.example.AuctionOuterClass.Seller.InAuction archive) {
        }

        public Seller.State_InAuction __initFromArchiveBytes(byte[] archiveBytes)
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.AuctionOuterClass.Seller.InAuction archive = org.hyperledger.fabric.example.AuctionOuterClass.Seller.InAuction.parseFrom(archiveBytes);
            initFromArchive(archive);
            return this;
        }

        public org.hyperledger.fabric.example.AuctionOuterClass.Seller.InAuction archive()
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.AuctionOuterClass.Seller.InAuction.Builder builder = org.hyperledger.fabric.example.AuctionOuterClass.Seller.InAuction.newBuilder();
            return builder.build();
        }
    }

    public enum State_Seller {
        Sold,
        Unsold,
        InAuction;
    }

    public class State_Sold {

        public void initFromArchive(org.hyperledger.fabric.example.AuctionOuterClass.Seller.Sold archive) {
        }

        public Seller.State_Sold __initFromArchiveBytes(byte[] archiveBytes)
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.AuctionOuterClass.Seller.Sold archive = org.hyperledger.fabric.example.AuctionOuterClass.Seller.Sold.parseFrom(archiveBytes);
            initFromArchive(archive);
            return this;
        }

        public org.hyperledger.fabric.example.AuctionOuterClass.Seller.Sold archive()
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.AuctionOuterClass.Seller.Sold.Builder builder = org.hyperledger.fabric.example.AuctionOuterClass.Seller.Sold.newBuilder();
            return builder.build();
        }
    }

    public class State_Unsold {

        public void initFromArchive(org.hyperledger.fabric.example.AuctionOuterClass.Seller.Unsold archive) {
        }

        public Seller.State_Unsold __initFromArchiveBytes(byte[] archiveBytes)
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.AuctionOuterClass.Seller.Unsold archive = org.hyperledger.fabric.example.AuctionOuterClass.Seller.Unsold.parseFrom(archiveBytes);
            initFromArchive(archive);
            return this;
        }

        public org.hyperledger.fabric.example.AuctionOuterClass.Seller.Unsold archive()
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.AuctionOuterClass.Seller.Unsold.Builder builder = org.hyperledger.fabric.example.AuctionOuterClass.Seller.Unsold.newBuilder();
            return builder.build();
        }
    }
}
