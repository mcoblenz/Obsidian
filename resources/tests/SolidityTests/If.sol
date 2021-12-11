// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract If {
    function main(int y) public pure returns (int) {
        int x = 0;
        if (true) {
            x = 1 + y;
        }
        return x;
    }
}