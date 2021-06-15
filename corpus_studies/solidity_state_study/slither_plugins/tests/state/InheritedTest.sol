// SPDX-License-Identifier: BSD-3
pragma solidity ^0.8.4;

contract A {
    enum State {A, B, C}
    State state;

    modifier inState(State s) {
      require(state == s);
      _;
    }
}

contract B is A {
    uint x;
    address owner;
    
    function moveToB() public inState(State.A) {
      state = State.B;
    }
}
