pragma solidity ^0.4.0;
import "./insurer.sol";
import "./policyrecord.sol";


contract InsuranceService {
    Insurer[] public insurers;
    address public owner;
    mapping (address => PolicyRecord[]) public pendingPolicies;
    mapping (address => PolicyRecord) public activePolicies;

    constructor() public {
        owner = msg.sender;
    }
    
    function requestBids(uint info) public returns (Policy[]){
        Policy[] memory policies = new Policy[](insurers.length);
        PolicyRecord[] memory pending = new PolicyRecord[](insurers.length);
        
        for (uint i = 0; i < insurers.length; i++) {
            InsuranceBid insBid = insurers[i].requestBid(info); //info for now is just int
            uint cost = insBid.cost();
            uint expirationTime = insBid.expirationTime();
            Money payout = insBid.payout();
            
            Policy policy = new Policy(cost, expirationTime);
            PolicyRecord record = new PolicyRecord(policy, payout); 
            
            policies[i] = policy;
            pending[i] = record;
        }
        
        pendingPolicies[msg.sender] = pending;
        
        return policies;
    }
    
    function buyPolicy(Policy policy, Money money, uint i) public returns (Money){
        require(policy.cost() == money.amount(), "Not enough money to buy policy.");
        require(policy.currentState() == Policy.States.Offered, "Policy must be Offered in order to purchase.");
        
        PolicyRecord pendingPolicy = pendingPolicies[msg.sender][i]; // assuming i is index of the Policy...


        if (now > policy.expirationTime()) {
            Money refund = pendingPolicy.refund();
            // have to find insurer from insurers and give refund...
            policy.expire();
            return money;
        } else {
            uint cost = policy.cost();
            Money payment = money.getAmountOfMoney(cost);
            pendingPolicy.activate(payment);
            activePolicies[msg.sender] = pendingPolicy;
            policy.activate;
            return money;
        }

    }
    
    function addInsurer(Insurer insurer) public {
        insurers.push(insurer);
    }
}