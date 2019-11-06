pragma solidity >=0.5.11;

//Finish the vote transaction by transitioning the state of a Voter to the required state.

contract Voter {
    string name;
    bool citizen;

    enum State { Eligible, Ineligible, Registered, FinishedVoting }
    State state;

    constructor(string memory n, bool citizenship) public {
        name = n;
        citizen = citizenship;
        if (!citizen) {
            state = State.Ineligible;
        }
        else {
            state = State.Eligible;
        }
    }

    // Transitions the voter from Registered to FinishedVoting.
    function vote() public {
        // TODO
    }

    // Transitions the voter from Eligible to either Registered or Ineligible.
    function register() public {
        //...
        state = State.Registered; // Always becomes Registered for now. TODO.
    }
}