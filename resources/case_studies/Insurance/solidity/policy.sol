pragma solidity ^0.4.0;
contract Policy {
    enum States {Offered, Active, Expired}
    States public currentState;
    uint public cost;
    uint public expirationTime;
    
    constructor (uint _cost, uint _expirationTime) public {
        cost = _cost;
        expirationTime = _expirationTime;
        currentState = States.Offered;
    }
    
    function activate() public {
        require(currentState == States.Offered, "Can't call activate() on Policy not in Offered state.");
        currentState = States.Active;
        cost = 0;
        expirationTime = 0;
    }
    
    function expire() public {
        require(currentState == States.Offered, "Can't call expire() on Policy not in Offered state.");
        currentState = States.Expired;
        cost = 0;
        expirationTime = 0;
    }
}