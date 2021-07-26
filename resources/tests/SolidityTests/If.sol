// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract If {
    function iftest() public pure {
        int x;
        if (true) {
            x = 1;
        }
        return;
    }
    
    function main() public pure returns (int) {
        int x = 0;
        if (true) {
            x = 1;
        }
        return x;
    }
}