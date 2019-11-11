pragma solidity >=0.5.11;

import "./betting_utils.sol";

contract Casino {
    Game currentGame; //The game that is currently being played
    BetList bets; //The bets for the current game being played

    constructor(Game g, BetList b) public {
        currentGame = g;
        bets = b;
    }

    //TODO: Add your code here.
}