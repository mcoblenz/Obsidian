pragma solidity ^0.4.0;
import "./money.sol";
import "./insurancebid.sol";
import "./bank.sol";


contract Insurer {
    Money public mon;
    Bank public bank;
    address public owner;
    
    constructor(Bank _bank, Money _money) public {
        bank = _bank;
        mon = _money;
        owner = msg.sender;
    }
    
    // TODO: Calculate bids using some needed information, for now just int
    function requestBid(uint i) public returns (InsuranceBid) {
        uint costOfBid = i + 4;
        uint costOfPayout = i + 6;

        // available for 24 hours
        uint twentyFourHours = 24 * 3600000;
        uint expirationTime = now + twentyFourHours;

        // buy token (to ensure money has been given)
        Money m = mon.getAmountOfMoney(costOfPayout);

        InsuranceBid bid = new InsuranceBid(expirationTime, costOfBid, m);

        return bid;        
    }
    
    
    function receiveRefund(Money m) public {
        mon.addMoney(m);
    }
}