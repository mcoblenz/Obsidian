// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.4;

contract Foo {
    uint x;
    
    constructor() {
        x = 0;
    }

    function bar() public view returns (uint) {
        if (x == 0) {
            return 0; 
        }
        else {
            return 1;
        }
    }
}
