pragma solidity ^0.4.18;

contract B {
    uint x = 5;
    function B() {

    }
}

contract C {

}

contract A is B, C {
    enum State {A, B, C, D, E}
    uint y;
    address owner;
    State state;

    function A() B() public {
        owner = msg.sender;
        x = 10;
        y = x + x + 10;

        state = State.C;

        y = 10;
    }

    function foo() public {
        require(state == State.C);

        state = State.A;
    }
}
