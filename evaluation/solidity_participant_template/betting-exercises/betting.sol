pragma solidity >=0.5.11;

import "./betting_utils.sol";

contract Casino {
    Money money;
    Game currentGame; //The game that is currently being played
    BetList bets; //The bets for the current game being played

    constructor(Money m, Game g, BetList b) public {
        money = m;
        currentGame = g;
        bets = b;
    }

    //TODO: Add your code here.
}