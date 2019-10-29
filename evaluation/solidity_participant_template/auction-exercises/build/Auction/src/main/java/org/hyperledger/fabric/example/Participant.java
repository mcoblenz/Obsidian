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
import org.hyperledger.fabric.example.Auction-solnativeOuterClass.MoneyOrGUID;
import org.hyperledger.fabric.example.Auction-solnativeOuterClass.ParticipantOrGUID;

public class Participant
    implements ObsidianSerialized
{
    private String __guid;
    private boolean __modified;
    private boolean __loaded;
    public Money money;
    static HashSet<String> transactionsWithOwnedReceiversAtBeginning;
    static HashSet<String> transactionsWithOwnedReceiversAtEnd;
    public boolean __isInsideInvocation = false;
    private boolean __constructorReturnsOwned = true;

    public Participant(String __guid_) {
        __modified = false;
        __loaded = false;
        __guid = __guid_;
    }

    public Participant(SerializationState __st)
        throws ObsidianRevertException
    {
        new_Participant(__st);
    }

    public Participant() {
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

    public void receivePayment(Money m, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new ReentrancyException("/Users/mcoblenz/code/Obsidian/evaluation/solidity_participant_template/auction-exercises/auction-solnative.obs", 0);
            } else {
                __isInsideInvocation = true;
                this.money.mergeMoney(m, __st);
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    private void new_Participant(SerializationState __st) {
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
        if (transName.equals("receivePayment")) {
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
                this.receivePayment(m, __st);
            } else {
                System.err.println("Wrong number of arguments in invocation.");
                throw new WrongNumberOfArgumentsException("receivePayment", args.length, 1);
            }
        } else {
            throw new NoSuchTransactionException();
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
        org.hyperledger.fabric.example.Auction-solnativeOuterClass.ParticipantOrGUID.Builder builder = org.hyperledger.fabric.example.Auction-solnativeOuterClass.ParticipantOrGUID.newBuilder();
        builder.setObj(this.archive());
        ParticipantOrGUID wrappedObject = builder.build();
        return wrappedObject.toByteArray();
    }

    public org.hyperledger.fabric.example.Auction-solnativeOuterClass.Participant archive()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.Auction-solnativeOuterClass.Participant.Builder builder = org.hyperledger.fabric.example.Auction-solnativeOuterClass.Participant.newBuilder();
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
        org.hyperledger.fabric.example.Auction-solnativeOuterClass.Participant archive = ((org.hyperledger.fabric.example.Auction-solnativeOuterClass.Participant) archiveIn);
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

    public Participant __initFromArchiveBytes(byte[] archiveBytes, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.Auction-solnativeOuterClass.Participant archive = org.hyperledger.fabric.example.Auction-solnativeOuterClass.Participant.parseFrom(archiveBytes);
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
        new_Participant(__st);
        return new byte[ 0 ] ;
    }

    @Override
    public boolean constructorReturnsOwnedReference() {
        return __constructorReturnsOwned;
    }
}
