// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract IfThenElse {
    function main(int y) public pure returns (int) {
        int x = 9;
        if (false) {
            x = 50;
        }
        else {
            x = 90 + y;
        }
        return x;
    }
}