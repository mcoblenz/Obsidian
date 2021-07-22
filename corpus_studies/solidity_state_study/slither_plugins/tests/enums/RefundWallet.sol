// SPDX-License-Identifier: BSD-3
pragma solidity ^0.8.4;

contract RefundWallet {
    enum State {Active, Success, Refunding, Closed}
    State public state;

    uint total;

    modifier inState(State _state) {
        require(state == _state);
        _;
    }

    constructor() {
        state = State.Active;
        total = 0;
    }

    function deposit() external payable {
        require(state == State.Active || state == State.Success);
        total += msg.value;
    }

    function saleSuccessful() external inState(State.Active) {
        state = State.Success;
    }

    function enableRefunds() external {
        require(state != State.Refunding && state != State.Closed);
        state = State.Refunding;
    }

    function refund() external inState(State.Refunding) {
        total = 0;
    }

    function close() external inState(State.Success) {
        state = State.Closed;
    }

    function doWhileClosed() external inState(State.Closed) {
        
    }
}