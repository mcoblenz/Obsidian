pragma solidity >=0.5.11;

//Fill in the code in the /* TODO */ below
//Note: s is a Share owned by another ShareHolder that is passed as a parameter to the receiveShare transaction, and will be set to the field "share"

contract Share {

}

contract ShareHolder {
    Share share; // owned

    constructor() public {
        share = new Share();
    }

    function receiveShare(Share s) public { // TODO
        //...
    }

    function checkShareValue(Share s) public {
        //...
    }
}