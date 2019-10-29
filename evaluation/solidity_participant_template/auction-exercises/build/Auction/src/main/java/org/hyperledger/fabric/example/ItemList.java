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
import org.hyperledger.fabric.example.AuctionOuterClass.ItemListOrGUID;
import org.hyperledger.fabric.example.AuctionOuterClass.ItemOrGUID;

public class ItemList
    implements ObsidianSerialized
{
    private State_ItemList __state;
    private ItemList.State_Empty __stateEmpty;
    private ItemList.State_HasItems __stateHasItems;
    private String __guid;
    private boolean __modified;
    private boolean __loaded;
    private boolean __constructorReturnsOwned = false;
    public ItemList next;
    public Item item;
    static HashSet<String> transactionsWithOwnedReceiversAtBeginning;
    static HashSet<String> transactionsWithOwnedReceiversAtEnd;
    public boolean __isInsideInvocation = false;

    public ItemList(String __guid_) {
        __modified = false;
        __loaded = false;
        __guid = __guid_;
    }

    public ItemList(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        new_ItemList(__st);
    }

    public ItemList() {
        __modified = true;
        __loaded = false;
    }

    public State_ItemList getState(SerializationState __st)
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
        if (this.getState(null) == State_ItemList.Empty) {
            __stateEmpty = null;
        }
        if (this.getState(null) == State_ItemList.HasItems) {
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
        if (__nextIsInScope()) {
            if (!checked.contains(next)) {
                result.addAll(next.__resetModified(checked));
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

    public boolean __nextIsInScope()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        return Arrays.asList(new String[] {"HasItems"}).contains(this.getState(null).toString());
    }

    public boolean __itemIsInScope()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        return Arrays.asList(new String[] {"HasItems"}).contains(this.getState(null).toString());
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

    private void new_ItemList(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __constructorReturnsOwned = true;
        __oldStateToNull();
        __stateEmpty = new ItemList.State_Empty();
        __state = State_ItemList.Empty;
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

    public void append(Item it, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/participant_template/auction-exercises/auction.obs", 0);
            } else {
                __isInsideInvocation = true;
                ItemList ilist = new ItemList(__st);
                __oldStateToNull();
                __stateHasItems = new ItemList.State_HasItems();
                this.item = it;
                __modified = true;
                this.next = ilist;
                __modified = true;
                __state = State_ItemList.HasItems;
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
            transactionsWithOwnedReceiversAtBeginning.add("append");
        }
        return transactionsWithOwnedReceiversAtBeginning.contains(methodName);
    }

    @Override
    public boolean methodReceiverIsOwnedAtEnd(String methodName) {
        if (transactionsWithOwnedReceiversAtEnd == null) {
            transactionsWithOwnedReceiversAtEnd = new HashSet<String>();
            transactionsWithOwnedReceiversAtBeginning.add("append");
        }
        return transactionsWithOwnedReceiversAtEnd.contains(methodName);
    }

    public byte[] run(SerializationState __st, String transName, byte[][] args)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, NoSuchTransactionException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        byte[] returnBytes = new byte[ 0 ] ;
        if (transName.equals("append")) {
            if (args.length == 1) {
                byte[] decoded0 = Base64 .getDecoder().decode(args[ 0 ]);
                ItemOrGUID archive0 = ItemOrGUID.parseFrom(decoded0);
                Item unarchivedObj0;
                if (archive0 .getEitherCase().equals((org.hyperledger.fabric.example.AuctionOuterClass.ItemOrGUID.EitherCase.GUID))) {
                    unarchivedObj0 = ((Item) __st.loadContractWithGUID(__st.getStub(), archive0 .getGuid(), true, false));
                } else {
                    unarchivedObj0 = new Item();
                    unarchivedObj0 .initFromArchive(archive0 .getObj(), __st);
                }
                Item it = unarchivedObj0;
                this.append(it, __st);
            } else {
                System.err.println("Wrong number of arguments in invocation.");
                throw new WrongNumberOfArgumentsException("append", args.length, 1);
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
        org.hyperledger.fabric.example.AuctionOuterClass.ItemListOrGUID.Builder builder = org.hyperledger.fabric.example.AuctionOuterClass.ItemListOrGUID.newBuilder();
        builder.setObj(this.archive());
        ItemListOrGUID wrappedObject = builder.build();
        return wrappedObject.toByteArray();
    }

    public org.hyperledger.fabric.example.AuctionOuterClass.ItemList archive()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.AuctionOuterClass.ItemList.Builder builder = org.hyperledger.fabric.example.AuctionOuterClass.ItemList.newBuilder();
        builder.setGuid(__guid);
        if (next!= null) {
            String nextArchiveID = next.__getGUID();
            builder.setNext(nextArchiveID);
        }
        if (item!= null) {
            String itemArchiveID = item.__getGUID();
            builder.setItem(itemArchiveID);
        }
        if (State_ItemList.Empty == this.getState(null)) {
            __stateEmpty.archive();
            builder.setStateEmpty(__stateEmpty.archive());
        }
        if (State_ItemList.HasItems == this.getState(null)) {
            __stateHasItems.archive();
            builder.setStateHasItems(__stateHasItems.archive());
        }
        return builder.build();
    }

    public void initFromArchive(Object archiveIn, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.AuctionOuterClass.ItemList archive = ((org.hyperledger.fabric.example.AuctionOuterClass.ItemList) archiveIn);
        __guid = archive.getGuid();
        if (archive.getStateCase().equals((org.hyperledger.fabric.example.AuctionOuterClass.ItemList.StateCase.STATEEMPTY))) {
            __state = State_ItemList.Empty;
        }
        if (archive.getStateCase().equals((org.hyperledger.fabric.example.AuctionOuterClass.ItemList.StateCase.STATEHASITEMS))) {
            __state = State_ItemList.HasItems;
        }
        if (__nextIsInScope()) {
            String nextID = archive.getNext();
            ItemList nextVal = ((ItemList) __st.getEntry(nextID));
            if (nextVal!= null) {
                next = nextVal;
            } else {
                next = new ItemList(nextID);
                __st.putEntry(nextID, next);
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

    public ItemList __initFromArchiveBytes(byte[] archiveBytes, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.AuctionOuterClass.ItemList archive = org.hyperledger.fabric.example.AuctionOuterClass.ItemList.parseFrom(archiveBytes);
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
        new_ItemList(__st);
        return new byte[ 0 ] ;
    }

    public class State_Empty {

        public void initFromArchive(org.hyperledger.fabric.example.AuctionOuterClass.ItemList.Empty archive) {
        }

        public ItemList.State_Empty __initFromArchiveBytes(byte[] archiveBytes)
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.AuctionOuterClass.ItemList.Empty archive = org.hyperledger.fabric.example.AuctionOuterClass.ItemList.Empty.parseFrom(archiveBytes);
            initFromArchive(archive);
            return this;
        }

        public org.hyperledger.fabric.example.AuctionOuterClass.ItemList.Empty archive()
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.AuctionOuterClass.ItemList.Empty.Builder builder = org.hyperledger.fabric.example.AuctionOuterClass.ItemList.Empty.newBuilder();
            return builder.build();
        }
    }

    public class State_HasItems {

        public void initFromArchive(org.hyperledger.fabric.example.AuctionOuterClass.ItemList.HasItems archive) {
        }

        public ItemList.State_HasItems __initFromArchiveBytes(byte[] archiveBytes)
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.AuctionOuterClass.ItemList.HasItems archive = org.hyperledger.fabric.example.AuctionOuterClass.ItemList.HasItems.parseFrom(archiveBytes);
            initFromArchive(archive);
            return this;
        }

        public org.hyperledger.fabric.example.AuctionOuterClass.ItemList.HasItems archive()
            throws com.google.protobuf.InvalidProtocolBufferException
        {
            org.hyperledger.fabric.example.AuctionOuterClass.ItemList.HasItems.Builder builder = org.hyperledger.fabric.example.AuctionOuterClass.ItemList.HasItems.newBuilder();
            return builder.build();
        }
    }

    public enum State_ItemList {
        Empty,
        HasItems;
    }
}
