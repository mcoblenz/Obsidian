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
        bool revealed;
        bytes32 secretChoice;
        Choice choice;

        address addr;
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
        playerOne.revealed = false;
        playerTwo.chosen = false;
        playerTwo.revealed = false;
    }

    function playerOneChoose(bytes32 choice) public {
        require(!playerOne.chosen, "Player 1 has already chosen!");
        playerOne.secretChoice = choice;
        playerOne.chosen = true;
        playerOne.addr = msg.sender;
    }

    function playerTwoChoose(bytes32 choice) public {
        require(!playerTwo.chosen, "Player 2 has already chosen!");
        playerTwo.secretChoice = choice;
        playerTwo.chosen = true;
        playerTwo.addr = msg.sender;
    }

    function playerOneReveal(Choice choice, bytes32 secret) public {
        playerOne = reveal(playerOne, choice, secret);
    }

    function playerTwoReveal(Choice choice, bytes32 secret) public {
        playerTwo = reveal(playerTwo, choice, secret);
    }

    function reveal(Player memory player, Choice choice, bytes32 secret) internal view returns (Player memory) {
        require(playerOne.chosen, "Player 1 has not chosen yet!");
        require(playerTwo.chosen, "Player 2 has not chosen yet!");
        require(!player.revealed, "Player has already revealed their choice!");

        require(player.addr == msg.sender, "Only Player can reveal their choice!");
        require(player.secretChoice == keccak256(abi.encodePacked(choice, secret)), "Player did not validly reveal their choice!");

        player.choice = choice;
        player.revealed = true;

        return player;
    }

    function winner() public view returns (Result) {
        require(playerOne.revealed, "Player 1 has not chosen yet!");
        require(playerTwo.revealed, "Player 2 has not chosen yet!");

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

