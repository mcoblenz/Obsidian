pragma solidity ^0.4.0;

contract Token {
    address minter;
    address public currentOwner;
    uint public amount;

    constructor(address _currentOwner, uint _amount) public {
        minter = msg.sender;
        currentOwner = _currentOwner;
        amount = _amount;
    }

}