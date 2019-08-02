pragma solidity >=0.4.22 <0.6.0;

contract Player {
    string move = "none";

    function getMove() public view returns (string memory) {
        return move;
    }

    function setMove(string memory newMove) public {

        if (compareStrings(newMove, "rock") || compareStrings(newMove, "paper") || compareStrings(newMove, "scissors")) {
            move = newMove;
        }
        else
        {
            revert("Possible moves: rock, paper, scissors");
        }
    }

    //From: https://ethereum.stackexchange.com/questions/30912/how-to-compare-strings-in-solidity
    //more at: https://solidity.readthedocs.io/en/latest/types.html#bytes-and-strings-as-arrays
    function compareStrings (string memory a, string memory b) public pure returns (bool) {
        return (keccak256(abi.encodePacked((a))) == keccak256(abi.encodePacked((b))));
    }
}

contract Game {
    Player p1 = new Player();
    Player p2 = new Player();
    
    //From: https://ethereum.stackexchange.com/questions/30912/how-to-compare-strings-in-solidity
    //more at: https://solidity.readthedocs.io/en/latest/types.html#bytes-and-strings-as-arrays
    function compareStrings (string memory a, string memory b) public pure returns (bool) {
        return (keccak256(abi.encodePacked((a))) == keccak256(abi.encodePacked((b))));
    }
    
    function player1Move(string memory a) public {
        p1.setMove(a);
    }
    
    function player2Move(string memory a) public {
        p2.setMove(a);
    }
    
    function play() public view returns (string memory) {
        
        string memory p1Move = p1.getMove();
        string memory p2Move = p2.getMove();
        
        if (compareStrings(p1Move, "none")) {
            return "Player 1 must play";
        }
        else if (compareStrings(p2Move, "none")) {
            return "Player 2 must play";
        }
        else if (compareStrings(p1Move, p2Move)) {
            return "Tie";
        }
        else if (compareStrings(p1Move, "rock")) {
            if (compareStrings(p2Move, "scissors")) {
                return "Player 1 wins!";
            }
            else {
                return "Player 2 wins!";
            }
        }
        else if (compareStrings(p1Move, "paper")) {
            if (compareStrings(p2Move, "rock")) {
                return "Player 1 wins!";
            }
            else {
                return "Player 2 wins!";
            }
        }
        else if (compareStrings(p1Move, "scissors")) {
            if (compareStrings(p2Move, "paper")) {
                return "Player 1 wins!";
            }
            else {
                return "Player 2 wins!";
            }
        }
        
    }

}