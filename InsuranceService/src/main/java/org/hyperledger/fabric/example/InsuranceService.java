package edu.cmu.cs.obsidian.generated_code;

import java.util.HashSet;
import java.util.Set;
import com.google.protobuf.InvalidProtocolBufferException;
import edu.cmu.cs.obsidian.chaincode.HyperledgerChaincodeBase;

public class InsuranceService
    extends HyperledgerChaincodeBase
    implements edu.cmu.cs.obsidian.chaincode.ObsidianSerialized
{
    private String __guid;
    private boolean __modified;
    private boolean __loaded;
    public Insurer insurers;
    public TimeService timeService;
    public PendingPolicyMap pendingPolicies;
    public ActivePolicyMap activePolicies;
    public Bank bank;
    public boolean __isInsideInvocation = false;

    public InsuranceService(String __guid_) {
        __modified = false;
        __loaded = false;
        __guid = __guid_;
    }

    public InsuranceService() {
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
        if (!checked.contains(insurers)) {
            result.addAll(insurers.__resetModified(checked));
        }
        if (!checked.contains(timeService)) {
            result.addAll(timeService.__resetModified(checked));
        }
        if (!checked.contains(pendingPolicies)) {
            result.addAll(pendingPolicies.__resetModified(checked));
        }
        if (!checked.contains(activePolicies)) {
            result.addAll(activePolicies.__resetModified(checked));
        }
        if (!checked.contains(bank)) {
            result.addAll(bank.__resetModified(checked));
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
        __guid = "InsuranceService";
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

    private void new_InsuranceService(edu.cmu.cs.obsidian.chaincode.SerializationState __st) {
        this.pendingPolicies = new PendingPolicyMap(__st);
        __modified = true;
        this.activePolicies = new ActivePolicyMap(__st);
        __modified = true;
        __guid = "InsuranceService";
        __modified = true;
        __loaded = true;
    }

    @Override
    protected void invokeConstructor(edu.cmu.cs.obsidian.chaincode.SerializationState __st) {
        this.new_InsuranceService(__st);
    }

    public void addInsurer(Insurer insurer, edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws InvalidProtocolBufferException, edu.cmu.cs.obsidian.chaincode.BadTransactionException, edu.cmu.cs.obsidian.chaincode.ReentrancyException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new edu.cmu.cs.obsidian.chaincode.ReentrancyException();
            } else {
                __isInsideInvocation = true;
                this.insurers = insurer;
                __modified = true;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    public Policy requestBids(java.math.BigInteger i, edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws InvalidProtocolBufferException, edu.cmu.cs.obsidian.chaincode.BadTransactionException, edu.cmu.cs.obsidian.chaincode.ReentrancyException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new edu.cmu.cs.obsidian.chaincode.ReentrancyException();
            } else {
                __isInsideInvocation = true;
                InsuranceBid insBid = this.insurers.requestBid(i, __st);
                java.math.BigInteger cost = insBid.getCost(__st);
                java.math.BigInteger expiration = insBid.getExpirationTime(__st);
                Money payout = insBid.getPayout(__st);
                Policy policy = new Policy(cost, expiration, __st);
                PolicyRecord pendingPolicy = new PolicyRecord(policy, payout, __st);
                this.pendingPolicies.set(policy, pendingPolicy, __st);
                return policy;
            }
        } finally {
            __isInsideInvocation = false;
        }
    }

    public Money buyPolicy(Policy policy, Money money, edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws InvalidProtocolBufferException, edu.cmu.cs.obsidian.chaincode.BadTransactionException, edu.cmu.cs.obsidian.chaincode.ReentrancyException
    {
        __restoreObject(__st);
        try {
            if (__isInsideInvocation) {
                throw new edu.cmu.cs.obsidian.chaincode.ReentrancyException();
            } else {
                __isInsideInvocation = true;
                if (this.timeService.getTime(__st).compareTo(policy.getExpirationTime(__st)) == 1) {
                    PolicyRecord pendingPolicy = this.pendingPolicies.get(policy, __st);
                    Money insurerRefund = pendingPolicy.refund(__st);
                    this.insurers.receiveRefund(insurerRefund, __st);
                    policy.expire(__st);
                    return money;
                } else {
                    PolicyRecord pendingPolicy = this.pendingPolicies.get(policy, __st);
                    java.math.BigInteger cost = policy.getCost(__st);
                    Money payment = money.getAmountOfMoney(cost, __st);
                    pendingPolicy.activate(payment, __st);
                    this.activePolicies.set(policy, pendingPolicy, __st);
                    policy.activate(__st);
                    return money;
                }
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
        new_InsuranceService(stub);
        return new byte[ 0 ] ;
    }

    public byte[] query(edu.cmu.cs.obsidian.chaincode.SerializationState stub, String transName, byte[][] args) {
        return new byte[ 0 ] ;
    }

    public byte[] getChaincodeID() {
        return new byte[ 0 ] ;
    }

    public static void main(String[] args) {
        InsuranceService instance = new InsuranceService();
        instance.delegatedMain(args);
    }

    public byte[] run(edu.cmu.cs.obsidian.chaincode.SerializationState stub, String transName, byte[][] args)
        throws InvalidProtocolBufferException, edu.cmu.cs.obsidian.chaincode.BadTransactionException, edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException, edu.cmu.cs.obsidian.chaincode.ReentrancyException
    {
        byte[] returnBytes = new byte[ 0 ] ;
        if (transName.equals("buyPolicy")) {
            Policy policy = new Policy().__initFromArchiveBytes(args[ 0 ]);
            Money money = new Money().__initFromArchiveBytes(args[ 1 ]);
            Money returnObj = buyPolicy(policy, money, stub);
            mapReturnedObject(returnObj);
            returnBytes = returnObj.__getGUID().getBytes();
        } else {
            if (transName.equals("requestBids")) {
                java.math.BigInteger i = new java.math.BigInteger(args[ 0 ]);
                Policy returnObj = requestBids(i, stub);
                mapReturnedObject(returnObj);
                returnBytes = returnObj.__getGUID().getBytes();
            } else {
                if (transName.equals("addInsurer")) {
                    Insurer insurer = new Insurer().__initFromArchiveBytes(args[ 0 ]);
                    addInsurer(insurer, stub);
                } else {
                    throw new edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException();
                }
            }
        }
        return returnBytes;
    }

    public byte[] __archiveBytes() {
        return this.archive().toByteArray();
    }

    public InsuranceServiceOuterClass.InsuranceService archive() {
        InsuranceServiceOuterClass.InsuranceService.Builder builder = InsuranceServiceOuterClass.InsuranceService.newBuilder();
        if (insurers!= null) {
            String insurersArchiveID = insurers.__getGUID();
            builder.setInsurers(insurersArchiveID);
        }
        if (timeService!= null) {
            String timeServiceArchiveID = timeService.__getGUID();
            builder.setTimeService(timeServiceArchiveID);
        }
        if (pendingPolicies!= null) {
            String pendingPoliciesArchiveID = pendingPolicies.__getGUID();
            builder.setPendingPolicies(pendingPoliciesArchiveID);
        }
        if (activePolicies!= null) {
            String activePoliciesArchiveID = activePolicies.__getGUID();
            builder.setActivePolicies(activePoliciesArchiveID);
        }
        if (bank!= null) {
            String bankArchiveID = bank.__getGUID();
            builder.setBank(bankArchiveID);
        }
        return builder.build();
    }

    public void initFromArchive(InsuranceServiceOuterClass.InsuranceService archive, edu.cmu.cs.obsidian.chaincode.SerializationState __st) {
        String insurersID = archive.getInsurers();
        Insurer insurersVal = ((Insurer) __st.getEntry(insurersID));
        if (insurersVal!= null) {
            insurers = insurersVal;
        } else {
            insurers = new Insurer(insurersID);
            __st.putEntry(insurersID, insurersVal);
        }
        String timeServiceID = archive.getTimeService();
        TimeService timeServiceVal = ((TimeService) __st.getEntry(timeServiceID));
        if (timeServiceVal!= null) {
            timeService = timeServiceVal;
        } else {
            timeService = new TimeService(timeServiceID);
            __st.putEntry(timeServiceID, timeServiceVal);
        }
        String pendingPoliciesID = archive.getPendingPolicies();
        PendingPolicyMap pendingPoliciesVal = ((PendingPolicyMap) __st.getEntry(pendingPoliciesID));
        if (pendingPoliciesVal!= null) {
            pendingPolicies = pendingPoliciesVal;
        } else {
            pendingPolicies = new PendingPolicyMap(pendingPoliciesID);
            __st.putEntry(pendingPoliciesID, pendingPoliciesVal);
        }
        String activePoliciesID = archive.getActivePolicies();
        ActivePolicyMap activePoliciesVal = ((ActivePolicyMap) __st.getEntry(activePoliciesID));
        if (activePoliciesVal!= null) {
            activePolicies = activePoliciesVal;
        } else {
            activePolicies = new ActivePolicyMap(activePoliciesID);
            __st.putEntry(activePoliciesID, activePoliciesVal);
        }
        String bankID = archive.getBank();
        Bank bankVal = ((Bank) __st.getEntry(bankID));
        if (bankVal!= null) {
            bank = bankVal;
        } else {
            bank = new Bank(bankID);
            __st.putEntry(bankID, bankVal);
        }
    }

    public InsuranceService __initFromArchiveBytes(byte[] archiveBytes, edu.cmu.cs.obsidian.chaincode.SerializationState __st)
        throws InvalidProtocolBufferException
    {
        InsuranceServiceOuterClass.InsuranceService archive = InsuranceServiceOuterClass.InsuranceService.parseFrom(archiveBytes);
        initFromArchive(archive, __st);
        __loaded = true;
        return this;
    }
}
