// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract PrimOpsNEq {
    function primopsneq() public pure returns (bool) {
        return (5 != 9);
    }
}