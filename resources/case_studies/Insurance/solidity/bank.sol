pragma solidity ^0.4.0;
import "./money.sol";

contract Bank {
    Money mon;
    address owner;
    mapping (address => uint) public balances;


    constructor() public {
        mon = new Money(0);
        owner = msg.sender;
    }
    
    function addMoney(uint amount) public {
        mon.addMoney(new Money(amount)); // probably doesn't work since both Moneys were made here ?
    }
    

    function send(address receiver, uint amount) public {
        require(balances[msg.sender] >= amount, "Not enough money to send.");
        balances[msg.sender] -= amount;
        balances[receiver] += amount;
    }
    
}