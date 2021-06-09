// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.4;

contract Foo {
    enum State {A, B, C}
    State state;
    uint x;
    
    constructor() {
        state = State.A;
        x = 0;
    }

    function moveToB() public {
        //require(x == 0);
        require(state == State.A);
        state = State.B;
    }

   /*
    struct S2 {
      uint x;
    }

    struct S1 {
      S2 s2;
    }

    S1 s1;

    function foo() public view returns (uint) {
      require(s1.s2.x == 0);
      return 0;
    }
    */
}
