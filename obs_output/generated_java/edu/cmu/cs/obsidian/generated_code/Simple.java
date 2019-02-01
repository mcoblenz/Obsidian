package edu.cmu.cs.obsidian.generated_code;

import java.util.HashSet;
import java.util.Set;
import com.google.protobuf.InvalidProtocolBufferException;
import edu.cmu.cs.obsidian.chaincode.BadTransactionException;
import edu.cmu.cs.obsidian.chaincode.HyperledgerChaincodeBase;
import edu.cmu.cs.obsidian.chaincode.ReentrancyException;

public class Simple
    extends HyperledgerChaincodeBase
    implements edu.cmu.cs.obsidian.chaincode.ObsidianSerialized
{
    private String __guid;
    private boolean __modified;
    private boolean __loaded;
    public boolean __isInsideInvocation = false;

    public Simple(String __guid_) {
        __modified = false;
        __loaded = false;
        __guid = __guid_;
    }

    public Simple() {
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
        __guid = "Simple";
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

    private void new_Simple(edu.cmu.cs.obsidian.chaincode.SerializationState __st) {
        __guid = "Simple";
        __modified = true;
        __loaded = true;
    }

    @Override
    protected void invokeConstructor(edu.cmu.cs.obsidian.chaincode.SerializationState __st) {
        this.new_Simple(__st);
    }

    public byte[] init(edu.cmu.cs.obsidian.chaincode.SerializationState stub, byte[][] args)
        throws InvalidProtocolBufferException
    {
        if (args.length!= 0) {
            throw new InvalidProtocolBufferException("Incorrect number of arguments to constructor.");
        }
        new_Simple(stub);
        return new byte[ 0 ] ;
    }

    public byte[] query(edu.cmu.cs.obsidian.chaincode.SerializationState stub, String transName, byte[][] args) {
        return new byte[ 0 ] ;
    }

    public byte[] getChaincodeID() {
        return new byte[ 0 ] ;
    }

    public static void main(String[] args) {
        Simple instance = new Simple();
        instance.delegatedMain(args);
    }

    public byte[] run(edu.cmu.cs.obsidian.chaincode.SerializationState stub, String transName, byte[][] args)
        throws InvalidProtocolBufferException, BadTransactionException, edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException, ReentrancyException
    {
        byte[] returnBytes = new byte[ 0 ] ;
        throw new edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException();
    }

    public byte[] __archiveBytes() {
        return this.archive().toByteArray();
    }

    public SimpleOuterClass.Simple archive() {
        SimpleOuterClass.Simple.Builder builder = SimpleOuterClass.Simple.newBuilder();
        return builder.build();
    }

    public void initFromArchive(SimpleOuterClass.Simple archive, edu.cmu.cs.obsidian.chaincode.SerializationState __st) {
    }

    public Simple __initFromArchiveBytes(byte[] archiveBytes, edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws InvalidProtocolBufferException
    {
        SimpleOuterClass.Simple archive = SimpleOuterClass.Simple.parseFrom(archiveBytes);
        initFromArchive(archive, __st);
        __loaded = true;
        return this;
    }
}
