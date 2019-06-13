pragma solidity >=0.4.22 <0.6.0;

contract RockPaperScissors {
    enum Choice {
        Rock,
        Paper,
        Scissors
    }

    enum Result {
        Tie,
        PlayerOneWin,
        PlayerTwoWin
    }

    struct Player {
        bool chosen;
        Choice choice;
    }

    Player playerOne;
    Player playerTwo;

    constructor() public {
        reset();
    }

    // Need this fallback function for some reason?
    function() external {}

    function reset() public {
        playerOne.chosen = false;
        playerTwo.chosen = false;
    }

    function playerOneChoose(Choice choice) public {
        require(!playerOne.chosen, "Player 1 has already chosen!");
        playerOne.choice = choice;
        playerOne.chosen = true;
    }

    function playerTwoChoose(Choice choice) public {
        // Does it matter if they choose in order?
        require(playerOne.chosen, "Player 1 has not chosen yet!");
        require(!playerTwo.chosen, "Player 2 has already chosen!");
        playerTwo.choice = choice;
        playerTwo.chosen = true;
    }

    function winner() public view returns (Result) {
        require(playerOne.chosen, "Player 1 has not chosen yet!");
        require(playerTwo.chosen, "Player 2 has not chosen yet!");

        if (playerOne.choice == playerTwo.choice) {
            return Result.Tie;
        }

        if (playerOne.choice == Choice.Rock) {
            if (playerTwo.choice == Choice.Paper) {
                return Result.PlayerTwoWin;
            } else {
                return Result.PlayerOneWin;
            }
        } else if (playerOne.choice == Choice.Paper) {
            if (playerTwo.choice == Choice.Scissors) {
                return Result.PlayerTwoWin;
            } else {
                return Result.PlayerOneWin;
            }
        } else {
            if (playerTwo.choice == Choice.Rock) {
                return Result.PlayerTwoWin;
            } else {
                return Result.PlayerOneWin;
            }
        }
    }
}

