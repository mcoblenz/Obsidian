// SPDX-License-Identifier: BSD-3
pragma solidity ^0.8.4;

contract Foo {
    enum State {A, B, C}
    State public state;
    uint public x;
    
    constructor() {
        state = State.C;
        x = 0;
    }

    
    //Should have an edge C --> A
    function moveToA() public {
        require(state == State.C);
        if (true) {
            state = State.A;
        }
        else {
            revert();
            //this assignment should be ignored
            state = State.B;
        }
    }
}
