package org.hyperledger.fabric.example;

import java.util.HashSet;
import java.util.Set;
import com.google.protobuf.ByteString;
import com.google.protobuf.InvalidProtocolBufferException;
import edu.cmu.cs.obsidian.chaincode.HyperledgerChaincodeBase;

public class Simple3
    extends HyperledgerChaincodeBase
    implements edu.cmu.cs.obsidian.chaincode.ObsidianSerialized
{
    private String __guid;
    private boolean __modified;
    private boolean __loaded;
    public java.math.BigInteger x = java.math.BigInteger.valueOf(0);
    public java.math.BigInteger y = java.math.BigInteger.valueOf(0);
    public boolean __isInsideInvocation = false;

    public Simple3() {
    }

    public Simple3(String __guid_) {
        __modified = false;
        __loaded = false;
        __guid = __guid_;
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
        __guid = "Simple3";
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

    public void t1(edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws InvalidProtocolBufferException, edu.cmu.cs.obsidian.chaincode.BadTransactionException, edu.cmu.cs.obsidian.chaincode.ReentrancyException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new edu.cmu.cs.obsidian.chaincode.ReentrancyException();
            } else {
                __isInsideInvocation = true;
                this.y = java.math.BigInteger.valueOf(1);
                __modified = true;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    public void t2(edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws InvalidProtocolBufferException, edu.cmu.cs.obsidian.chaincode.BadTransactionException, edu.cmu.cs.obsidian.chaincode.ReentrancyException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new edu.cmu.cs.obsidian.chaincode.ReentrancyException();
            } else {
                __isInsideInvocation = true;
                if (this.y.compareTo(java.math.BigInteger.valueOf(3)) == 1) {
                    this.y = java.math.BigInteger.valueOf(0);
                    __modified = true;
                } else {
                    throw new RuntimeException();
                }
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    private void new_Simple3(edu.cmu.cs.obsidian.chaincode.SerializationState __st) {
        x = java.math.BigInteger.valueOf(0);
        y = java.math.BigInteger.valueOf(0);
    }

    public byte[] init(edu.cmu.cs.obsidian.chaincode.SerializationState stub, byte[][] args)
        throws InvalidProtocolBufferException
    {
        if (args.length!= 0) {
            throw new InvalidProtocolBufferException("Incorrect number of arguments to constructor.");
        }
        return new byte[ 0 ] ;
    }

    public byte[] query(edu.cmu.cs.obsidian.chaincode.SerializationState stub, String transName, byte[][] args) {
        return new byte[ 0 ] ;
    }

    public byte[] getChaincodeID() {
        return new byte[ 0 ] ;
    }

    public static void main(String[] args) {
        Simple3 instance = new Simple3();
        instance.delegatedMain(args);
    }

    public byte[] run(edu.cmu.cs.obsidian.chaincode.SerializationState stub, String transName, byte[][] args)
        throws InvalidProtocolBufferException, edu.cmu.cs.obsidian.chaincode.BadTransactionException, edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException, edu.cmu.cs.obsidian.chaincode.ReentrancyException
    {
        byte[] returnBytes = new byte[ 0 ] ;
        if (transName.equals("t1")) {
            t1(stub);
        } else {
            if (transName.equals("t2")) {
                t2(stub);
            } else {
                throw new edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException();
            }
        }
        return returnBytes;
    }

    public byte[] __archiveBytes() {
        return this.archive().toByteArray();
    }

    public Simple3OuterClass.Simple3 archive() {
        Simple3OuterClass.Simple3.Builder builder = Simple3OuterClass.Simple3.newBuilder();
        if (x!= null) {
            builder.setX(ByteString.copyFrom(x.toByteArray()));
        }
        if (y!= null) {
            builder.setY(ByteString.copyFrom(y.toByteArray()));
        }
        return builder.build();
    }

    public void initFromArchive(Simple3OuterClass.Simple3 archive, edu.cmu.cs.obsidian.chaincode.SerializationState __st) {
        if (!archive.getX().isEmpty()) {
            x = new java.math.BigInteger(archive.getX().toByteArray());
        }
        if (!archive.getY().isEmpty()) {
            y = new java.math.BigInteger(archive.getY().toByteArray());
        }
    }

    public Simple3 __initFromArchiveBytes(byte[] archiveBytes, edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws InvalidProtocolBufferException
    {
        Simple3OuterClass.Simple3 archive = Simple3OuterClass.Simple3 .parseFrom(archiveBytes);
        initFromArchive(archive, __st);
        __loaded = true;
        return this;
    }
}
