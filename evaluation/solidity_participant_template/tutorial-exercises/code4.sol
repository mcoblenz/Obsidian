pragma solidity >=0.5.11;

//Write a Person contract that has an owned reference to a Wallet object. Person should also have an addMoney()
//transaction that takes in a Money parameter and passes it to its Wallet's receiveMoney() transaction.
//addMoney() should return the old money that was replaced

contract Money {
    int public amount;
}

contract Wallet {
    // m is owned
    Money m;

    constructor() public {
        m = new Money();
    }

    function spendMoney() public {
        //...
    }

    // mon is owned initially but unowned at the end.
    // Returns an owned reference.
    function replaceMoney(Money mon) public returns (Money) {
        Money temp = m;
        m = mon;
        return temp;
    }

}