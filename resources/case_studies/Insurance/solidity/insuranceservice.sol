pragma solidity ^0.4.0;
import "./insurer.sol";

contract InsuranceService {
    Insurer[] public insurers;
    address public owner;
    mapping (address => uint) public accounts;

    constructor() public {
        owner = msg.sender;
    }
    
    function requestBids(uint info) public returns (Bid[]){
        Bid[] memory bids = new Bid[](insurers.length);
        
        for (uint i = 0; i < insurers.length; i++) {
            BidTokenPair bidTokenPair = insurers[i].requestBid(info); //info for now is just int
            bids[i] = bidTokenPair.bid();
            accounts[insurers[i]] += bidTokenPair.token().amount();
        }
        
        return bids;
    }
    
    function buyBid(Bid bid, Token token) public {
        require(msg.sender == token.currentOwner(), "Can't buy bid with unowned tokens.");
        require(bid.cost() == token.amount(), "Not enough tokens to buy bid.");

        
        uint costOfPayout = bid.costOfPayout();
        address insurer = bid.insurer();
        require(accounts[insurer] == costOfPayout, "Insurer hasn't given correct payout.");
        
        // add payout to insured's account
        accounts[insurer] -= costOfPayout;
        accounts[msg.sender] += costOfPayout;
        
        bid.buy(msg.sender);
    }
}