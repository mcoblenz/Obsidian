pragma solidity >=0.5.11;

/*
The following program simulates an English auction; there are multiple Bidders
who each make a Bid for a single Item being sold by a Seller.
The highest Bidder will receive the Item for the price of the highest Bid.

NOTE: Unlike in a normal English auction, when a Bidder makes a Bid, they give the Money to the
Auction house IMMEDIATELY, and the Money is returned to that Bidder if another Bidder makes a higher Bid.
*/


contract Auction {
    address maxBidder; // the bidder who made the highest bid so far
    uint maxBidAmount;
    address payable seller;

    // Allow withdrawing previous bid money for bids that were outbid
    mapping(address => uint) pendingReturns;

    enum State { Open, BidsMade, Closed }
    State state;

    constructor(address payable s) public {
        seller = s;
        state = State.Open;
    }

    function bid() public payable {
        if (state == State.Open) {
            maxBidder = msg.sender;
            maxBidAmount = msg.value;
            state = State.BidsMade;
        }
        else {
            if (state == State.BidsMade) {
                if (msg.value > maxBidAmount) { //if the newBid is higher than the current Bid
                    //1. TODO: fill this in. You may call any other functions as needed.
                }
                else {
                    //2. TODO: return the newBid money to the bidder, since the newBid wasn't high enough. 
                    //You may call any other functions as needed.
                }
            }
            else {
                revert ("Can only make a bid on an open auction.");
            }
        }
    }

    function withdraw() public {
        uint amount = pendingReturns[msg.sender];
        if (amount > 0) {
            pendingReturns[msg.sender] = 0;

            msg.sender.transfer(amount);
        }
    }

    // This gets called by an administrator when the auction is closed.
    function finishBidding() public {
      if (state == State.BidsMade) {
        state = State.Closed;
        seller.transfer(maxBidAmount);
      }
      else {
         revert("Can only finishBidding in state BidsMade");
      }
    }
}