pragma solidity >=0.5.11;

contract Casino {
    //Represents a single bet
    struct Bet {
        int prediction; // The prediction for the winner
        uint wager; // The amount of money the Bettor put down on this BetPrediction
    }

    Game currentGame; //The game that is currently being played
    uint casinoPot; // Number of tokens owned by the Casino

    // Assume there's code that lets people buy and sell tokens. You don't need to write it.
    mapping (address => uint) tokenBalances;
    mapping (address => Bet) bets;
    mapping (address => bool) betsTaken;

    constructor(Game g) public {
        currentGame = g;
        casinoPot = 1000; // Assume the casino starts with plenty of tokens
    }


    //TODO: Add your code here.
}

//Represents a generic game
contract Game {
    enum State { BeforePlay, Playing, FinishedPlaying}
    State public state;
    int outcome; // In FinishedPlaying state, stores information about the outcome of the game.

    constructor() public {
        state = State.BeforePlay;
    }

    //Start the game
    function startPlaying() public {
        require(state == State.BeforePlay, "Wrong initial state.");
        state = State.Playing;
    }

    //Finish the game
    function finishPlaying() public {
        require(state == State.Playing, "Wrong initial state.");
        outcome = 42; // Just assume something simple for now.
        state = State.FinishedPlaying;
    }

    //Returns the outcome of the game
    function predictionMatchesOutcome(int prediction) public view returns (bool) {
        require(state == State.FinishedPlaying, "Wrong initial state.");
        //...
        return prediction == outcome;
    }
}