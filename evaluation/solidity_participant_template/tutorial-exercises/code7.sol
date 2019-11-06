pragma solidity >=0.5.11;

//1. Write code to complete the "buy" transaction declaration below.
//2. Complete the restock transaction above by transitioning the state of TinyVendingMachine.

contract Candy {
}

contract Coin {
}

contract CoinBag {
    // Takes ownership of c.
    function deposit(Coin c) public {
    }
}

contract TinyVendingMachine {
    CoinBag coinBin;
    Candy inventory; // only when in Full state

    enum State {Full, Empty}
    State state;

    // Starts Empty.
    constructor () public {
        coinBin = new CoinBag();
        state = State.Empty;
    }

    // Transitions the receiver from Empty to Full.
    // Consumes ownership of c.
    function restock(Candy c) public {
        require(state == State.Empty, "Can only restock in Empty state.");
        //2. TODO
    }

    // 3. TODO: write what happens to the ownership of c.
    // Returns Candy owned by the caller.
    function buy(Coin c) public returns (Candy) {
        require(state == State.Full, "Can only buy in Full state.");
        coinBin.deposit(c);
        Candy result = inventory;
        state = State.Empty;
        return result;
    }
}