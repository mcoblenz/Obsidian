// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract PrimOpsMod {
    function primopsmod() public pure returns (int) {
        return (13 % 8);
    }
}