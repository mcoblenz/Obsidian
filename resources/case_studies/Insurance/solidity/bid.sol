pragma solidity ^0.4.0;
contract Bid {
    enum States { Unpurchased, Purchased }
    States currentState;
    uint public expirationTime;
    uint public cost;
    uint public costOfPayout;
    address public insurer;
    address public insured;
    
    constructor (uint _expirationTime, uint _cost, uint _costOfPayout) public {
        insurer = msg.sender;
        insured = address(0);
        expirationTime = _expirationTime;
        cost = _cost;
        costOfPayout = _costOfPayout;
        currentState = States.Unpurchased;
    }
    
    function buy(address _insured) public {
        require(expirationTime > now, "Bid expired.");
        require(insured != address(0), "Bid is not sold, yet has an owner.");
        
        currentState = States.Purchased;
        insured = _insured;
    }
    
}