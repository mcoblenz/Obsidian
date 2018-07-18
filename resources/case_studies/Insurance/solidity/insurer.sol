pragma solidity ^0.4.0;
import "./money.sol";
import "./bidtokenpair.sol";
import "./bank.sol";


contract Insurer {
    Money public mon;
    Bank public bank;
    address public owner;
    
    constructor(Bank _bank) public {
        bank = _bank;
        owner = msg.sender;
    }
    
    function addMoney(uint amount) public {
        mon.addMoney(new Money(amount));
    }
    
    // TODO: Calculate bids using some needed information, for now just int
    function requestBid(uint i) public returns (BidTokenPair) {
        uint costOfBid = i + 4;
        uint costOfPayout = i + 6;

        // available for 24 hours
        uint twentyFourHours = 24 * 3600000;
        uint expirationTime = now + twentyFourHours;

        // buy token (to ensure money has been given)
        Money m = mon.getAmountOfMoney(costOfPayout);
        Token token = bank.buyToken(m, owner, costOfPayout);

        Bid bid = new Bid(expirationTime, costOfBid, costOfPayout);

        return new BidTokenPair(bid, token);        
    }
    
    // TODO return money for tokens??
    
}