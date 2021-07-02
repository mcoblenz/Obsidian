// SPDX-License-Identifier: BSD-3
pragma solidity ^0.8.4;

contract A {
    uint x;

    function getX() public view returns (uint) {
        return getX2();
    }
    
    function getX2() public view returns (uint) {
        return x;
    }
    
}
