// SPDX-License-Identifier: UNLICENSED
// Adapted from https://github.com/rrao88/blockly-afep/blob/0f58396eee49fd117e9ca45183011efebc3e69b3/code-example/SatislohContract.sol
pragma solidity ^0.8.4;

contract SatislohContract {

	enum State { Created, Ordered, Produced, InDelivery, Received, Inactive }
	State public state;
	address payable public seller;
	address public buyer;
	uint public value;
        address public carrier;


	constructor() {
		seller = payable(msg.sender);
		value = 100;
		state = State.Created;
	}

	modifier condition(bool _condition) {
		require(_condition);
		_;
	}
        
        modifier inCreatedState() {
            require(state == State.Created);
            _;
        }

	modifier inState(State _state) {
		require(state == _state);
		_;
	}

	modifier onlyBuyer() {
		require(msg.sender == buyer);
		_;
	}

	modifier onlySeller() {
		require(msg.sender == seller);
		_;
	}

	modifier onlyCarrier() {
		require(msg.sender == carrier);
		_;
	}

	event DeliverOrder();
	event PaySecondInstallment();
	event InstallMachine();

	function confirmOrderPlaced() inState(State.Created) payable public {
		buyer = msg.sender;
		state = State.Ordered;
		seller.transfer(address(this).balance);
	}

	function confirmOrderProduced() onlySeller inState(State.Ordered) public {
		emit PaySecondInstallment();
		state = State.Produced;
	}

	function confirmSecondInstallmentPaid() onlySeller inState(State.Produced) public payable {
		seller.transfer(address(this).balance);
		emit DeliverOrder();
	}

	function confirmOrderInDelivery() onlyCarrier inState(State.Produced) public {
		state = State.InDelivery;
	}

	function confirmOrderReceived() onlyBuyer inState(State.InDelivery) public {
		state = State.Received;
		emit InstallMachine();
	}

	function confirmOrderCompleted() onlyBuyer inState(State.Received) public {
		seller.transfer(address(this).balance);
		state = State.Inactive;
	}

}
