package org.hyperledger.fabric.example;

import java.math.BigInteger;
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
import org.hyperledger.fabric.example.BettingOuterClass.CasinoOrGUID;

public class Casino
    extends HyperledgerChaincodeBase
    implements ObsidianSerialized
{
    private String __guid;
    private boolean __modified;
    private boolean __loaded;
    private boolean __constructorReturnsOwned = false;
    public Money money;
    public Game currentGame;
    public BetList bets;
    static HashSet<String> transactionsWithOwnedReceiversAtBeginning;
    static HashSet<String> transactionsWithOwnedReceiversAtEnd;
    public boolean __isInsideInvocation = false;

    public Casino(String __guid_) {
        __modified = false;
        __loaded = false;
        __guid = __guid_;
    }

    public Casino(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        new_Casino(__st);
    }

    public Casino() {
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
        if (__currentGameIsInScope()) {
            if (!checked.contains(currentGame)) {
                result.addAll(currentGame.__resetModified(checked));
            }
        }
        if (__betsIsInScope()) {
            if (!checked.contains(bets)) {
                result.addAll(bets.__resetModified(checked));
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

    public boolean __currentGameIsInScope()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        return true;
    }

    public boolean __betsIsInScope()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        return true;
    }

    public void __restoreObject(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        __guid = "Casino";
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

    private void new_Casino(SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __constructorReturnsOwned = true;
        this.money = new Money(BigInteger.valueOf(100000), __st);
        __modified = true;
        this.currentGame = new Game(__st);
        __modified = true;
        this.bets = new BetList(__st);
        __modified = true;
        __guid = "Casino";
        __modified = true;
        __loaded = true;
        __st.putEntry(__guid, this);
        __st.mapReturnedObject(this, false);
    }

    @Override
    public boolean constructorReturnsOwnedReference() {
        return __constructorReturnsOwned;
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
        Casino instance = new Casino("Casino");
        instance.delegatedMain(args);
    }

    public byte[] run(SerializationState __st, String transName, byte[][] args)
        throws com.google.protobuf.InvalidProtocolBufferException, BadArgumentException, BadTransactionException, IllegalOwnershipConsumptionException, InvalidStateException, NoSuchTransactionException, ObsidianRevertException, ReentrancyException, StateLockException, WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        __st.mapReturnedObject(this, false);
        byte[] returnBytes = new byte[ 0 ] ;
        throw new NoSuchTransactionException();
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
        org.hyperledger.fabric.example.BettingOuterClass.CasinoOrGUID.Builder builder = org.hyperledger.fabric.example.BettingOuterClass.CasinoOrGUID.newBuilder();
        builder.setObj(this.archive());
        CasinoOrGUID wrappedObject = builder.build();
        return wrappedObject.toByteArray();
    }

    public org.hyperledger.fabric.example.BettingOuterClass.Casino archive()
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.BettingOuterClass.Casino.Builder builder = org.hyperledger.fabric.example.BettingOuterClass.Casino.newBuilder();
        builder.setGuid(__guid);
        if (money!= null) {
            String moneyArchiveID = money.__getGUID();
            builder.setMoney(moneyArchiveID);
        }
        if (currentGame!= null) {
            String currentGameArchiveID = currentGame.__getGUID();
            builder.setCurrentGame(currentGameArchiveID);
        }
        if (bets!= null) {
            String betsArchiveID = bets.__getGUID();
            builder.setBets(betsArchiveID);
        }
        return builder.build();
    }

    public void initFromArchive(Object archiveIn, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.BettingOuterClass.Casino archive = ((org.hyperledger.fabric.example.BettingOuterClass.Casino) archiveIn);
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
        if (__currentGameIsInScope()) {
            String currentGameID = archive.getCurrentGame();
            Game currentGameVal = ((Game) __st.getEntry(currentGameID));
            if (currentGameVal!= null) {
                currentGame = currentGameVal;
            } else {
                currentGame = new Game(currentGameID);
                __st.putEntry(currentGameID, currentGame);
            }
        }
        if (__betsIsInScope()) {
            String betsID = archive.getBets();
            BetList betsVal = ((BetList) __st.getEntry(betsID));
            if (betsVal!= null) {
                bets = betsVal;
            } else {
                bets = new BetList(betsID);
                __st.putEntry(betsID, bets);
            }
        }
        __loaded = true;
    }

    public Casino __initFromArchiveBytes(byte[] archiveBytes, SerializationState __st)
        throws com.google.protobuf.InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.BettingOuterClass.Casino archive = org.hyperledger.fabric.example.BettingOuterClass.Casino.parseFrom(archiveBytes);
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
        new_Casino(__st);
        return new byte[ 0 ] ;
    }
}
