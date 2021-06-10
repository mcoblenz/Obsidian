// SPDX-License-Identifier: BSD-3
pragma solidity ^0.8.4;

contract Foo {
    enum State {A, B, C}
    State state;
    uint x;
    
    constructor() {
        state = State.A;
        x = 0;
    }

    //We expect that this function will be marked as evidence of state since
    //it has a require statement, comparing a state variable to an enum value.
    function moveToB() public {
        require(state == State.A);
        state = State.B;
    }
}
