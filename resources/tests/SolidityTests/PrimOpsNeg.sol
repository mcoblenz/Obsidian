// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract PrimOpsNeg {
    function primopsneg(int y) public pure returns (int) {
        return (-y);
    }
}