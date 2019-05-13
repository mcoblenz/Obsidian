package edu.cmu.cs.obsidian.generated_code;

import java.util.HashSet;
import java.util.Set;
import com.google.protobuf.InvalidProtocolBufferException;
import edu.cmu.cs.obsidian.chaincode.HyperledgerChaincodeBase;

public class Test
    extends HyperledgerChaincodeBase
    implements edu.cmu.cs.obsidian.chaincode.ObsidianSerialized
{
    private String __guid;
    private boolean __modified;
    private boolean __loaded;
    public boolean __isInsideInvocation = false;

    public Test() {
    }

    public Test(String __guid_) {
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
        throws InvalidProtocolBufferException, edu.cmu.cs.obsidian.chaincode.BadTransactionException, edu.cmu.cs.obsidian.chaincode.ReentrancyException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new edu.cmu.cs.obsidian.chaincode.ReentrancyException();
            } else {
                __isInsideInvocation = true;
                new IOImpl(__st).print("hello", __st);
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    private void new_Test(edu.cmu.cs.obsidian.chaincode.SerializationState __st) {
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
        Test instance = new Test();
        instance.delegatedMain(args);
    }

    public byte[] run(edu.cmu.cs.obsidian.chaincode.SerializationState stub, String transName, byte[][] args)
        throws InvalidProtocolBufferException, edu.cmu.cs.obsidian.chaincode.BadTransactionException, edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException, edu.cmu.cs.obsidian.chaincode.ReentrancyException
    {
        byte[] returnBytes = new byte[ 0 ] ;
        if (transName.equals("Hello")) {
            Hello(stub);
        } else {
            throw new edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException();
        }
        return returnBytes;
    }

    public byte[] __archiveBytes() {
        return this.archive().toByteArray();
    }

    public TestFFIOuterClass.Test archive() {
        TestFFIOuterClass.Test.Builder builder = TestFFIOuterClass.Test.newBuilder();
        return builder.build();
    }

    public void initFromArchive(TestFFIOuterClass.Test archive, edu.cmu.cs.obsidian.chaincode.SerializationState __st) {
    }

    public Test __initFromArchiveBytes(byte[] archiveBytes, edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws InvalidProtocolBufferException
    {
        TestFFIOuterClass.Test archive = TestFFIOuterClass.Test.parseFrom(archiveBytes);
        initFromArchive(archive, __st);
        __loaded = true;
        return this;
    }
}
