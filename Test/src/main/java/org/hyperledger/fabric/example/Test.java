package org.hyperledger.fabric.example;

import java.util.Set;
import com.google.protobuf.InvalidProtocolBufferException;
import edu.cmu.cs.obsidian.chaincode.HyperledgerChaincodeBase;
import edu.cmu.cs.obsidian.stdlib.IO;
import org.hyperledger.fabric.example.TestFFIOuterClass.TestOrGUID;

public class Test
    extends HyperledgerChaincodeBase
    implements edu.cmu.cs.obsidian.chaincode.ObsidianSerialized
{
    private String __guid;
    private boolean __modified;
    private boolean __loaded;
    static java.util.HashSet<String> transactionsWithOwnedReceiversAtBeginning;
    static java.util.HashSet<String> transactionsWithOwnedReceiversAtEnd;
    public boolean __isInsideInvocation = false;

    public Test(String __guid_) {
        __modified = false;
        __loaded = false;
        __guid = __guid_;
    }

    public Test(edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws edu.cmu.cs.obsidian.chaincode.ObsidianRevertException
    {
        new_Test(__st);
    }

    public Test() {
    }

    public String __getGUID() {
        return __guid;
    }

    public Set<edu.cmu.cs.obsidian.chaincode.ObsidianSerialized> __resetModified(Set<edu.cmu.cs.obsidian.chaincode.ObsidianSerialized> checked) {
        checked.add(this);
        Set<edu.cmu.cs.obsidian.chaincode.ObsidianSerialized> result = new java.util.HashSet<edu.cmu.cs.obsidian.chaincode.ObsidianSerialized>();
        if (!__loaded) {
            return result;
        }
        if (this.__modified) {
            result.add(this);
        }
        __modified = false;
        return result;
    }

    public void __restoreObject(edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws InvalidProtocolBufferException
    {
        __guid = "Test";
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

    public void Hello(edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws InvalidProtocolBufferException, edu.cmu.cs.obsidian.chaincode.BadTransactionException, edu.cmu.cs.obsidian.chaincode.InvalidStateException, edu.cmu.cs.obsidian.chaincode.ObsidianRevertException, edu.cmu.cs.obsidian.chaincode.ReentrancyException, edu.cmu.cs.obsidian.chaincode.StateLockException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new edu.cmu.cs.obsidian.chaincode.ReentrancyException("resources/tests/compilerTests/testFFI.obs", 0);
            } else {
                __isInsideInvocation = true;
                new IO(__st).print("hello");
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    private void new_Test(edu.cmu.cs.obsidian.chaincode.SerializationState __st) {
        __guid = "Test";
        __st.flushEntries();
    }

    @Override
    public boolean methodReceiverIsOwnedAtBeginning(String methodName) {
        if (transactionsWithOwnedReceiversAtBeginning == null) {
            transactionsWithOwnedReceiversAtBeginning = new java.util.HashSet<String>();
        }
        return transactionsWithOwnedReceiversAtBeginning.contains(methodName);
    }

    @Override
    public boolean methodReceiverIsOwnedAtEnd(String methodName) {
        if (transactionsWithOwnedReceiversAtEnd == null) {
            transactionsWithOwnedReceiversAtEnd = new java.util.HashSet<String>();
        }
        return transactionsWithOwnedReceiversAtEnd.contains(methodName);
    }

    public byte[] query(edu.cmu.cs.obsidian.chaincode.SerializationState __st, String transName, byte[][] args) {
        return new byte[ 0 ] ;
    }

    public byte[] getChaincodeID() {
        return new byte[ 0 ] ;
    }

    public static void main(String[] args) {
        Test instance = new Test("Test");
        instance.delegatedMain(args);
    }

    public byte[] run(edu.cmu.cs.obsidian.chaincode.SerializationState __st, String transName, byte[][] args)
        throws InvalidProtocolBufferException, edu.cmu.cs.obsidian.chaincode.BadArgumentException, edu.cmu.cs.obsidian.chaincode.BadTransactionException, edu.cmu.cs.obsidian.chaincode.IllegalOwnershipConsumptionException, edu.cmu.cs.obsidian.chaincode.InvalidStateException, edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException, edu.cmu.cs.obsidian.chaincode.ObsidianRevertException, edu.cmu.cs.obsidian.chaincode.ReentrancyException, edu.cmu.cs.obsidian.chaincode.StateLockException, edu.cmu.cs.obsidian.chaincode.WrongNumberOfArgumentsException
    {
        __restoreObject(__st);
        __st.mapReturnedObject(this, false);
        byte[] returnBytes = new byte[ 0 ] ;
        if (transName.equals("Hello")) {
            if (args.length == 0) {
                Hello(__st);
            } else {
                System.err.println("Wrong number of arguments in invocation.");
                throw new edu.cmu.cs.obsidian.chaincode.WrongNumberOfArgumentsException("Hello", args.length, 0);
            }
        } else {
            throw new edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException();
        }
        return returnBytes;
    }

    public byte[] __archiveBytes() {
        return this.archive().toByteArray();
    }

    public byte[] __wrappedArchiveBytes() {
        org.hyperledger.fabric.example.TestFFIOuterClass.TestOrGUID.Builder builder = org.hyperledger.fabric.example.TestFFIOuterClass.TestOrGUID.newBuilder();
        builder.setObj(this.archive());
        TestOrGUID wrappedObject = builder.build();
        return wrappedObject.toByteArray();
    }

    public org.hyperledger.fabric.example.TestFFIOuterClass.Test archive() {
        org.hyperledger.fabric.example.TestFFIOuterClass.Test.Builder builder = org.hyperledger.fabric.example.TestFFIOuterClass.Test.newBuilder();
        builder.setGuid(__guid);
        return builder.build();
    }

    public void initFromArchive(org.hyperledger.fabric.example.TestFFIOuterClass.Test archive, edu.cmu.cs.obsidian.chaincode.SerializationState __st) {
        __guid = archive.getGuid();
    }

    public Test __initFromArchiveBytes(byte[] archiveBytes, edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.TestFFIOuterClass.Test archive = org.hyperledger.fabric.example.TestFFIOuterClass.Test.parseFrom(archiveBytes);
        initFromArchive(archive, __st);
        __loaded = true;
        return this;
    }

    public byte[] init(edu.cmu.cs.obsidian.chaincode.SerializationState __st, byte[][] args)
        throws InvalidProtocolBufferException, edu.cmu.cs.obsidian.chaincode.BadArgumentException, edu.cmu.cs.obsidian.chaincode.IllegalOwnershipConsumptionException, edu.cmu.cs.obsidian.chaincode.ObsidianRevertException, edu.cmu.cs.obsidian.chaincode.WrongNumberOfArgumentsException
    {
        if (args.length!= 0) {
            throw new InvalidProtocolBufferException("Incorrect number of arguments to constructor.");
        }
        new_Test(__st);
        return new byte[ 0 ] ;
    }

    @Override
    public boolean constructorReturnsOwnedReference() {
        return true;
    }
}
