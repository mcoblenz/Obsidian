pragma solidity ^0.4.0;
contract Money {
    uint public amount;
    address public owner;
    
    constructor(uint _amount) public {
        owner = msg.sender;
        amount = _amount;
    }
    
    function addMoney(Money m) public {
        // validate m's owner somehow?
        // here we can ensure that the owner of the money isn't continuously sending money to themselves
        require(owner != m.owner(), "Can't add the same money to current money.");
        amount += m.amount(); // have to make amounts public for this
        delete(m);
    }
    
    function getAmountOfMoney(uint _amount) public returns (Money) {
        require(_amount <= amount, "Requesting too much money.");
        amount -= _amount;
        return Money(_amount);
    }
    
}