package edu.cmu.cs.obsidian.generated_code;

import java.util.HashSet;
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
    public boolean __isInsideInvocation = false;

    public IntContainer(String __guid_) {
        __modified = false;
        __loaded = false;
        __guid = __guid_;
    }

    public IntContainer() {
    }

    public String __getGUID() {
        return __guid;
    }

    public Set<edu.cmu.cs.obsidian.chaincode.ObsidianSerialized> __resetModified(Set<edu.cmu.cs.obsidian.chaincode.ObsidianSerialized> checked) {
        checked.add(this);
        Set<edu.cmu.cs.obsidian.chaincode.ObsidianSerialized> result = new HashSet<edu.cmu.cs.obsidian.chaincode.ObsidianSerialized>();
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

    private void new_IntContainer(edu.cmu.cs.obsidian.chaincode.SerializationState __st) {
        this.x = java.math.BigInteger.valueOf(5);
        __modified = true;
        this.s = "aaa";
        __modified = true;
        this.b = true;
        __modified = true;
        __guid = "IntContainer";
        __modified = true;
        __loaded = true;
    }

    @Override
    protected void invokeConstructor(edu.cmu.cs.obsidian.chaincode.SerializationState __st) {
        this.new_IntContainer(__st);
    }

    public java.math.BigInteger setX(java.math.BigInteger newX, edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws InvalidProtocolBufferException, edu.cmu.cs.obsidian.chaincode.BadTransactionException, edu.cmu.cs.obsidian.chaincode.ReentrancyException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new edu.cmu.cs.obsidian.chaincode.ReentrancyException();
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

    public byte[] init(edu.cmu.cs.obsidian.chaincode.SerializationState stub, byte[][] args)
        throws InvalidProtocolBufferException
    {
        if (args.length!= 0) {
            throw new InvalidProtocolBufferException("Incorrect number of arguments to constructor.");
        }
        new_IntContainer(stub);
        return new byte[ 0 ] ;
    }

    public byte[] query(edu.cmu.cs.obsidian.chaincode.SerializationState stub, String transName, byte[][] args) {
        return new byte[ 0 ] ;
    }

    public byte[] getChaincodeID() {
        return new byte[ 0 ] ;
    }

    public static void main(String[] args) {
        IntContainer instance = new IntContainer();
        instance.delegatedMain(args);
    }

    public byte[] run(edu.cmu.cs.obsidian.chaincode.SerializationState stub, String transName, byte[][] args)
        throws InvalidProtocolBufferException, edu.cmu.cs.obsidian.chaincode.BadTransactionException, edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException, edu.cmu.cs.obsidian.chaincode.ReentrancyException
    {
        byte[] returnBytes = new byte[ 0 ] ;
        if (transName.equals("setX")) {
            java.math.BigInteger newX = new java.math.BigInteger(args[ 0 ]);
            java.math.BigInteger returnObj = setX(newX, stub);
            returnBytes = returnObj.toByteArray();
        } else {
            throw new edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException();
        }
        return returnBytes;
    }

    public byte[] __archiveBytes() {
        return this.archive().toByteArray();
    }

    public IntContainerOuterClass.IntContainer archive() {
        IntContainerOuterClass.IntContainer.Builder builder = IntContainerOuterClass.IntContainer.newBuilder();
        if (x!= null) {
            builder.setX(ByteString.copyFrom(x.toByteArray()));
        }
        if (s!= null) {
            builder.setS(s);
        }
        builder.setB(b);
        return builder.build();
    }

    public void initFromArchive(IntContainerOuterClass.IntContainer archive, edu.cmu.cs.obsidian.chaincode.SerializationState __st) {
        if (!archive.getX().isEmpty()) {
            x = new java.math.BigInteger(archive.getX().toByteArray());
        }
        if (!archive.getS().isEmpty()) {
            s = archive.getS();
        }
        b = archive.getB();
    }

    public IntContainer __initFromArchiveBytes(byte[] archiveBytes, edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws InvalidProtocolBufferException
    {
        IntContainerOuterClass.IntContainer archive = IntContainerOuterClass.IntContainer.parseFrom(archiveBytes);
        initFromArchive(archive, __st);
        __loaded = true;
        return this;
    }
}
