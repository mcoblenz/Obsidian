pragma solidity ^0.4.0;
import "./bid.sol";
import "./token.sol";

contract BidTokenPair {
    Bid public bid;
    Token public token;
    
    constructor(Bid _bid, Token _token) public {
        bid = _bid;
        token = _token;
    }
}