// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract IfThenElse {
    function ifthenelse() public pure {
        int x;
        if (true) {
            x = 1;
        }
        else {
            x = 0;
        }
        return;
    }
    
    function main() public pure returns (int) {
        int x = 9;
        if (false) {
            x = 50;
        }
        else {
            x = 90;
        }
        return x;
    }
}