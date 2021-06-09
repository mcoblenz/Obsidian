// SPDX-License-Identifier: UNLICENSED
// Adapted from https://github.com/wpalombini/Lottery-Blockchain/blob/99c13461bb670ae92a0ab6d58c0ca145fc84b3fb/src/contracts/LotteryContract.sol
pragma solidity ^0.8.4;

pragma experimental ABIEncoderV2;


contract LotteryContract {
    //IGovernanceContract public governance;

    enum GameStateEnum {
        OPEN,
        CLOSED,
        PROCESSING_WINNERS,
        PAYING_WINNERS,
        PAYING_WINNERS_COMPLETED
    }

    GameStateEnum public gameState = GameStateEnum.CLOSED;
    
    uint256 public bettingPrice = 10000;
    
    address payable admin;
    
    uint public currentGameId = 1;
    uint totalBets = 0;
    address payable[] winners;
    
    struct Game {
        uint id;
        uint randomNumber;
        uint totalBetAmount;
        BettingNumbers drawnNumbers;
    }
    
    mapping (uint => Game) public games;
    
    struct Bet {
        uint id;
        address payable player;
        uint bettingAmount;
        uint gameId;
        BettingNumbers bettingNumbers;
    }
    
    struct BettingNumbers {
        uint8 n1;
        uint8 n2;
        uint8 n3;
        uint8 n4;
    }
    
    Bet[] public bets;
    
    /**
     * Constructor inherits VRFConsumerBase
     * 
     * Network: Kovan
     * Chainlink VRF Coordinator address: 0xdD3782915140c8f3b190B5D67eAc6dc5760C46E9
     * LINK token address:                0xa36085F69e2889c224210F603D836748e7dC0088
     * Key Hash: 0x6c3699283bda56ad74f6b855546325b68d482e983852a7a82979cc4807b641f4
     */
    constructor(address _governance) {
        admin = payable(msg.sender);
        //governance = IGovernanceContract(_governance);
    }
    
    modifier activeGameRequired() {
        require(gameState == GameStateEnum.OPEN, "There are no active games accepting bets");
        require(games[currentGameId].id == currentGameId, "cannot find game");
        _;
    }

    modifier noActiveGameRequired() {
        require(gameState == GameStateEnum.CLOSED, "There is an active game running");
        _;
    }
    
    modifier adminRequired() {
        require(msg.sender == admin, "Only admin has access to this resource");
        _;
    }
  
    modifier validBettingNumbersRequired(uint8 n1, uint8 n2, uint8 n3, uint8 n4) {
        require(n1 <= 9, "First digit must be less than 10");
        require(n2 <= 9, "Second digit must be less than 10");
        require(n3 <= 9, "Third digit must be less than 10");
        require(n4 <= 9, "Fourth digit must be less than 10");
        _;
    }

    modifier validBettingPrice() {
        require(msg.value == bettingPrice, "Invalid betting price");
        _;
    }

    function getBalance() public view adminRequired returns (uint) {
        return address(this).balance;
    }

    function withdrawBalance(address payable _to) payable public adminRequired noActiveGameRequired {
        require(_to != address(0) || _to != address(0x0), "The destination address is invalid");

        _to.transfer(address(this).balance);
    }
    
    function startGame() public adminRequired {
        // ensure there are no active games
        require(gameState == GameStateEnum.CLOSED, "There is an active game already");
        
        // ensure there are no outstanding bets
        require(bets.length == 0, "There are outstanding bets");
        
        // activate game
        gameState = GameStateEnum.OPEN;
        
        // currentGameId + 1
        currentGameId++;
        
        // add new game to games mapping
        games[currentGameId] = Game(currentGameId, 0, 0, BettingNumbers(0, 0, 0, 0));
    }
    
    function endGame() private {
        
        // reset bets array
        delete bets;

        // reset winners array
        winners = new address payable[](0);
        
        // deactivate game
        gameState = GameStateEnum.CLOSED;
    }
    
    function placeBet(uint8 n1, uint8 n2, uint8 n3, uint8 n4) payable public activeGameRequired validBettingPrice validBettingNumbersRequired(n1, n2, n3, n4) {
        totalBets++;
        
        games[currentGameId].totalBetAmount += msg.value;
        
        BettingNumbers memory bettingNumbers = BettingNumbers(n1, n2, n3, n4);
        
        Bet memory bet = Bet(totalBets, payable(msg.sender), msg.value, currentGameId, bettingNumbers);
        
        bets.push(bet);
    }
    
    function drawNumbers(uint256 seed) public adminRequired activeGameRequired returns (bytes32 requestId) {
        gameState = GameStateEnum.PROCESSING_WINNERS;
        return getRandomNumber(seed);
    }

    function getRandomNumber(uint256 userProvidedSeed) private returns (bytes32 requestId) {
        require(gameState == GameStateEnum.PROCESSING_WINNERS, "Invalid game state to fetch random number");
        return bytes32(0);
        //return IRandomnessContract(governance.randomness()).randomNumber(userProvidedSeed);
    }

    function fulfillRandomNumber(uint256 _randomness) external {
        require(gameState == GameStateEnum.PROCESSING_WINNERS, "Invalid game state to process random number");
        games[currentGameId].randomNumber = _randomness;

        processDrawnNumbers();
    }
    
    function processDrawnNumbers() private {
        require(gameState == GameStateEnum.PROCESSING_WINNERS, "Invalid game state to process drawn numbers");

        uint256 randomNumber = games[currentGameId].randomNumber;

        uint8 n1 = uint8(randomNumber % 10000 / 1000);
        uint8 n2 = uint8(randomNumber % 1000 / 100);
        uint8 n3 = uint8(randomNumber % 100 / 10);
        uint8 n4 = uint8(randomNumber % 10);

        games[currentGameId].drawnNumbers = BettingNumbers(n1, n2, n3, n4);

        gameState = GameStateEnum.PAYING_WINNERS;
        payoutPrizes();
    }

    function payoutPrizes() private {
        require(gameState == GameStateEnum.PAYING_WINNERS, "Invalid game state to payout prizes");

        findWinners();

        payoutWinners();

        gameState = GameStateEnum.PAYING_WINNERS_COMPLETED;

        endGame();
    }

    function findWinners() private {
        for (uint i; i < bets.length; i++) {
            bool winner = bets[i].bettingNumbers.n1 == games[currentGameId].drawnNumbers.n1
                && bets[i].bettingNumbers.n2 == games[currentGameId].drawnNumbers.n2
                && bets[i].bettingNumbers.n3 == games[currentGameId].drawnNumbers.n3
                && bets[i].bettingNumbers.n4 == games[currentGameId].drawnNumbers.n4;

            if (winner) {
                winners.push(bets[i].player);
            }
        }
    }

    function payoutWinners() private {
        // If there are winners...
        if (winners.length > 0) { 
            // ...take 10% profit
            uint256 profit = games[currentGameId].totalBetAmount * 10 / 100;

            uint256 payablePrize = games[currentGameId].totalBetAmount - profit;

            uint256 individualPayablePrize = payablePrize / winners.length;

            for (uint i; i < winners.length; i++) {
                winners[i].transfer(individualPayablePrize);
            }
        }
    }
}
