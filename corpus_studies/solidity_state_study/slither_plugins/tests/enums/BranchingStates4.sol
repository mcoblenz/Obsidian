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

    modifier inStates(State a, State b) {
        require(state == a || state == b);
        _;
    }

    //Should have two edges, A -> C and B -> C
    function moveToC() public inStates(State.A, State.B) {
        while (true) {
            x++;
        }

        for (uint i = 0; i < 10; i++) {
            x++;
        }
        state = State.C;
    }
}
