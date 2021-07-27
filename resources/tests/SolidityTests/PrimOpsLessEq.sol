// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract PrimOpsLessEq {
    function primopslesseq() public pure returns (bool) {
        return (9 <= 19);
    }
}