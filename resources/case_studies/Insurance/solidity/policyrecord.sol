pragma solidity ^0.4.0;
import "./policy.sol";
import "./money.sol";


contract PolicyRecord {
    Policy public policy;
    enum States {Pending, Active, Expired}
    States public currentState;
    Money money;


    constructor (Policy _policy, Money _money) public {
        policy = _policy;
        money = _money;
        currentState = States.Pending;
    }

    function refund() public returns (Money) {
        require(currentState == States.Pending, "PolicyRecord must be Pending to give refund.");
        currentState = States.Expired;
        Money returnMoney = money;
        delete(money);
        return returnMoney;
    }

    function activate(Money policyCost) public {
        require(currentState == States.Pending, "PolicyRecord must be Pending to activate.");
        money.addMoney(policyCost);
        currentState = States.Active;
    }
}