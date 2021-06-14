// SPDX-License-Identifier: MIT
// https://github.com/supersudh/solidity-learning/blob/a5a818705ffad7a31eb8d20c46a792101c6178d2/Purchase/contracts/Purchase.sol
pragma solidity >=0.4.22 <0.9.0;

// Buyer ops
// .. -> performPurchase()
// .. -> confirmReceipt()

// Seller ops
// create Contract with value -> constructor()
// abort -> abortSale()

contract Purchase {
    address payable public buyer;
    address payable public seller;
    uint256 public value;

    enum State {Created, Locked, Inactive, Released}
    State state;

    modifier condition(bool _condition) {
        require(_condition);
        _;
    }

    modifier onlyBuyer() {
        require(
            msg.sender == buyer,
            "You must be the buyer to perform this op"
        );
        _;
    }

    modifier onlySeller() {
        require(
            msg.sender == seller,
            "You must be the seller to perform this op"
        );
        _;
    }

    modifier inState(State _state) {
        require(state == _state);
        _;
    }

    event Aborted();
    event Refunded();
    event Purchased();
    event ConfirmReceipt();

    constructor() payable {
        seller = payable(msg.sender);
        value = msg.value / 2;
        require(value * 2 == msg.value, "Escrow even numbered value");
    }

    function abort() public onlySeller inState(State.Created) {
        emit Aborted();
        state = State.Inactive;
        seller.transfer(address(this).balance);
    }

    function performPurchase()
        public
        payable
        condition(msg.value == value * 2)
        inState(State.Created)
    {
        emit Purchased();
        buyer = payable(msg.sender);
        state = State.Locked;
    }

    function confirmReceipt() public onlyBuyer inState(State.Locked) {
        emit ConfirmReceipt();
        state = State.Released;
        buyer.transfer(value);
    }

    function refundSeller() public onlySeller inState(State.Released) {
      emit Refunded();
      state = State.Inactive;
      seller.transfer(value * 3);
    }
}
