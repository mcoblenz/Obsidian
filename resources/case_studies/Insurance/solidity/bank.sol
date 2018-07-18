pragma solidity ^0.4.0;
import "./money.sol";
import "./token.sol";

contract Bank {
    Money mon;
    address owner;
    mapping (address => uint) public tokenBalances;


    constructor() public {
        mon = new Money(0);
        owner = msg.sender;
    }
    
    function addMoney(uint amount) public {
        mon.addMoney(new Money(amount)); // probably doesn't work since both Moneys were made here ?
    }
    
    function buyToken(Money m, address receiver, uint amount) public returns (Token) {
        require(m.amount() == amount, "Not enough money for tokens requested.");
        require(owner == msg.sender, "Cannot make tokens if not bank.");

        mon.addMoney(m);
        tokenBalances[receiver] += amount;
        
        return new Token(receiver, amount);
    }

    function send(address receiver, uint amount) public {
        require(tokenBalances[msg.sender] >= amount, "Not enough tokens to send.");
        tokenBalances[msg.sender] -= amount;
        tokenBalances[receiver] += amount;
    }
    
    //TODO Get money from token?
}