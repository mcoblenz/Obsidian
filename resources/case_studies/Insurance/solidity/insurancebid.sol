pragma solidity ^0.4.0;
import "./money.sol";


contract InsuranceBid {
    uint public expirationTime;
    uint public cost;
    Money public payout;
    address public insurer;

    constructor (uint _expirationTime, uint _cost, Money _payout) public {
        insurer = msg.sender;
        expirationTime = _expirationTime;
        cost = _cost;
        payout = _payout;
    }
    
}