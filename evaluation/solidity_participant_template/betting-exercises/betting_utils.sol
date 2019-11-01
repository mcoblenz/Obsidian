pragma solidity >=0.5.11;


//Represents the money being traded in this program
contract Money {
    int amount; //Amount that this Money object holds

    constructor(int amt) public {
        amount = amt;
    }

    //Add additional Money
    function mergeMoney(Money m) public {
         amount = amount + m.getAmount();
    }

    //Returns the amount of this Money
    function getAmount() public view returns (int) {
        return amount;
    }

    // DANGER: for internal use only!
    function addAmount(int amt) public {
        amount += amt;
    }

    //Use some amount of this Money
    function splitMoneyInto(int amt, Money otherMoney) public {
        if (amt > amount) {
            revert ("Can't split out more money than is available in a given Money object.");
        } else {
            amount = amount - amt;
            otherMoney.addAmount(amt);
        }
    }
}

//Represents the bet prediction of a Bettor
contract BetPrediction {
    string public predictedOutcome; //the Bettor's prediction of the winning outcome
    int public betAmount; //the amount of money the Bettor put down on this BetPrediction

    constructor(string memory predOut, int amount) public {
        predictedOutcome = predOut;
        betAmount = amount;
    }
}


//Represents a person betting on a game
contract Bettor {
    Money money; //Total Money the Bettor owns

    constructor(Money m) public {
        money = m;
    }

    //Take some Money out of the Bettor's total money
    function withdrawMoney(int amount) public returns (Money) {
        Money newMoney = new Money(0);
        money.splitMoneyInto(amount, newMoney);
        return newMoney;
    }

    //Adds Money to the Bettor's total money
    function receiveMoney(Money m) public {
        money.mergeMoney(m);
    }
}

//Represents a single bet
contract Bet {
    Bettor public bettor; //The Bettor making the bet
    BetPrediction public prediction; //The BetPrediction for the winner

    constructor(Bettor b, BetPrediction p) public {
        bettor = b;
        prediction = p;
    }
}

//Represents a container of Bets
contract BetList {
    Bet[] bets;

    //Add a new Bet to the end of the list
    function append(Bet newB) public {
        bets.push(newB);
    }

    //remove a Bet with a given Bettor reference, and return that Bet
    function popSpecific(Bettor bettor) public returns (Bet) {
        for (uint i = 0; i < bets.length; i++) {
            if (bets[i].bettor() == bettor) {
                Bet result = bets[i];
                bets[i] = bets[bets.length - 1];
                delete bets[bets.length - 1];
                bets.length--;
                return result;
            }
        }
        revert("No matching bets in list");
    }

    //returns true if a Bet has been placed by a Bettor
    function contains(Bettor bettor) public view returns (bool) {
        for (uint i = 0; i < bets.length; i++) {
            if (bets[i].bettor() == bettor) {
                return true;
            }
        }
    }
}

//Represents a generic game
contract Game {
    enum State { BeforePlay, Playing, FinishedPlaying}
    State state;

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
        state = State.FinishedPlaying;
    }

    //Returns the outcome of the game
    function calculateOutcome() public view returns (string memory) {
        require(state == State.FinishedPlaying, "Wrong initial state.");
        //...
        return ""; //Always returns empty string for now
    }
}