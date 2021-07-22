// SPDX-License-Identifier: BSD-3
pragma solidity ^0.8.4;

contract Foo {
    // Transition graph:
    // A --> B <--> C
    //
    // D
    // Initial state: B
    // Unreachable states: A, D
    enum State {A, B, C, D}
    State public state;
    uint public x;
    
    constructor() {
        state = State.B;
        x = 0;
    }

    modifier inState(State s) {
        require(state == s);
        _;
    }

    function goToB() public {
        require(state == State.A || state == State.C);
        state = State.B;
    }

    function goToC() public inState(State.B) {
        state = State.C;
    }
}
