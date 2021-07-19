// SPDX-License-Identifier: BSD-3
pragma solidity ^0.8.4;

contract Foo {
    enum State {A, B, C}
    State public state = State.C;
    uint public x;
    
    constructor() {
        state = State.B;
        x = 0;
    }

    //The detector should find two possible ending states,
    //A --> B and
    //A --> A.
    function moveToB() public {
        require(state == State.A);
        if (false) {
            state = State.B;
        }
        else if (true) {
            state = State.A;
        }
        
        x = 5;
    }

    /*
    //A -> B, B -> C, C -> A
    function f() public {
        if (state == State.A) {
            state = State.B;
        }
        else if (state == State.B) {
            state = State.C;
        }
        else {
            state = State.A;
        }
    }
    */
}
