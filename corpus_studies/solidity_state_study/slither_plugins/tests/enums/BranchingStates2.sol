// SPDX-License-Identifier: BSD-3
pragma solidity ^0.8.4;

contract Foo {
    enum State {A, B, C}
    State public state;
    uint public x;
    
    constructor() {
        state = State.A;
        x = 0;
    }

    //Only the last assignment should be considered.
    //I expect multiple assignments like this are fairly rare in practice...
    //Should have single edge B --> C
    function moveToC() public {
        require(state == State.B);
        state = State.B;
        state = State.C;
        while (true) {
            state = State.A;
        }
        state = State.C;
    }
}
