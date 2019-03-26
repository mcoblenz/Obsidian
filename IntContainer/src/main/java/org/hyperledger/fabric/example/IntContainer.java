package org.hyperledger.fabric.example;

import java.nio.charset.StandardCharsets;
import java.util.Set;
import com.google.protobuf.ByteString;
import com.google.protobuf.InvalidProtocolBufferException;
import edu.cmu.cs.obsidian.chaincode.HyperledgerChaincodeBase;

public class IntContainer
    extends HyperledgerChaincodeBase
    implements edu.cmu.cs.obsidian.chaincode.ObsidianSerialized
{
    private String __guid;
    private boolean __modified;
    private boolean __loaded;
    public java.math.BigInteger x = java.math.BigInteger.valueOf(0);
    public String s = "";
    public boolean b = false;
    static java.util.HashSet<String> transactionsWithOwnedReceivers;
    public boolean __isInsideInvocation = false;

    public IntContainer(String __guid_) {
        __modified = false;
        __loaded = false;
        __guid = __guid_;
    }

    public IntContainer(edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws edu.cmu.cs.obsidian.chaincode.ObsidianRevertException
    {
        new_IntContainer(__st);
    }

    public IntContainer() {
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

    public boolean __xIsInScope() {
        return true;
    }

    public boolean __sIsInScope() {
        return true;
    }

    public boolean __bIsInScope() {
        return true;
    }

    public void __restoreObject(edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws InvalidProtocolBufferException
    {
        __guid = "IntContainer";
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

    private void new_IntContainer(edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws edu.cmu.cs.obsidian.chaincode.ObsidianRevertException
    {
        this.x = java.math.BigInteger.valueOf(5);
        __modified = true;
        this.s = "aaa";
        __modified = true;
        this.b = true;
        __modified = true;
        __guid = "IntContainer";
        __modified = true;
        __loaded = true;
        __st.putEntry(__guid, this);
    }

    @Override
    public boolean constructorReturnsOwnedReference() {
        return false;
    }

    public java.math.BigInteger setX(java.math.BigInteger newX, edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws InvalidProtocolBufferException, edu.cmu.cs.obsidian.chaincode.BadTransactionException, edu.cmu.cs.obsidian.chaincode.InvalidStateException, edu.cmu.cs.obsidian.chaincode.ObsidianRevertException, edu.cmu.cs.obsidian.chaincode.ReentrancyException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new edu.cmu.cs.obsidian.chaincode.ReentrancyException("resources/tests/compilerTests/IntContainer.obs", 12);
            } else {
                __isInsideInvocation = true;
                java.math.BigInteger oldX = this.x;
                this.x = newX;
                __modified = true;
                return oldX;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    @Override
    public boolean methodReceiverIsOwned(String methodName) {
        if (transactionsWithOwnedReceivers == null) {
            transactionsWithOwnedReceivers = new java.util.HashSet<String>();
        }
        return transactionsWithOwnedReceivers.contains(methodName);
    }

    public byte[] query(edu.cmu.cs.obsidian.chaincode.SerializationState __st, String transName, byte[][] args) {
        return new byte[ 0 ] ;
    }

    public byte[] getChaincodeID() {
        return new byte[ 0 ] ;
    }

    public static void main(String[] args) {
        IntContainer instance = new IntContainer("IntContainer");
        instance.delegatedMain(args);
    }

    public byte[] run(edu.cmu.cs.obsidian.chaincode.SerializationState __st, String transName, byte[][] args)
        throws InvalidProtocolBufferException, edu.cmu.cs.obsidian.chaincode.BadArgumentException, edu.cmu.cs.obsidian.chaincode.BadTransactionException, edu.cmu.cs.obsidian.chaincode.IllegalOwnershipConsumptionException, edu.cmu.cs.obsidian.chaincode.InvalidStateException, edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException, edu.cmu.cs.obsidian.chaincode.ObsidianRevertException, edu.cmu.cs.obsidian.chaincode.ReentrancyException, edu.cmu.cs.obsidian.chaincode.WrongNumberOfArgumentsException
    {
        byte[] returnBytes = new byte[ 0 ] ;
        if (transName.equals("setX")) {
            if (args.length == 1) {
                java.math.BigInteger unmarshalledInt0 = new java.math.BigInteger(new String(args[ 0 ], StandardCharsets.UTF_8));
                if (unmarshalledInt0 == null) {
                    throw new edu.cmu.cs.obsidian.chaincode.BadArgumentException(new String(args[ 0 ], StandardCharsets.UTF_8));
                }
                java.math.BigInteger newX = unmarshalledInt0;
                java.math.BigInteger returnObj = setX(newX, __st);
                returnBytes = returnObj.toString().getBytes(StandardCharsets.UTF_8);
            } else {
                System.err.println("Wrong number of arguments in invocation.");
                throw new edu.cmu.cs.obsidian.chaincode.WrongNumberOfArgumentsException("setX", args.length, 1);
            }
        } else {
            throw new edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException();
        }
        return returnBytes;
    }

    public byte[] __archiveBytes() {
        return this.archive().toByteArray();
    }

    public org.hyperledger.fabric.example.IntContainerOuterClass.IntContainer archive() {
        org.hyperledger.fabric.example.IntContainerOuterClass.IntContainer.Builder builder = org.hyperledger.fabric.example.IntContainerOuterClass.IntContainer.newBuilder();
        builder.setGuid(__guid);
        if (x!= null) {
            builder.setX(ByteString.copyFrom(x.toByteArray()));
        }
        if (s!= null) {
            builder.setS(s);
        }
        builder.setB(b);
        return builder.build();
    }

    public void initFromArchive(org.hyperledger.fabric.example.IntContainerOuterClass.IntContainer archive, edu.cmu.cs.obsidian.chaincode.SerializationState __st) {
        __guid = archive.getGuid();
        if (__xIsInScope()) {
            if (!archive.getX().isEmpty()) {
                x = new java.math.BigInteger(archive.getX().toByteArray());
            }
        }
        if (__sIsInScope()) {
            if (!archive.getS().isEmpty()) {
                s = archive.getS();
            }
        }
        if (__bIsInScope()) {
            b = archive.getB();
        }
    }

    public IntContainer __initFromArchiveBytes(byte[] archiveBytes, edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws InvalidProtocolBufferException
    {
        org.hyperledger.fabric.example.IntContainerOuterClass.IntContainer archive = org.hyperledger.fabric.example.IntContainerOuterClass.IntContainer.parseFrom(archiveBytes);
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
        new_IntContainer(__st);
        return new byte[ 0 ] ;
    }
}
