// SPDX-License-Identifier: BSD-3
pragma solidity ^0.8.4;

contract Deactivation {
    bool stopped;
    uint public x;
    uint y;
    
    constructor() {
        x = 5;
    }

    function stop() public {
        require(stopped == false);
        stopped = true;
    }

    function addToX() public {
        require(!stopped);
        y++;
        x += y;
    }
}
