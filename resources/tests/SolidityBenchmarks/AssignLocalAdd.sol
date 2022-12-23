// SPDX-License-Identifier: BSD

pragma solidity >=0.8.0;

contract AssignLocalAdd {
    function assignlocaladd() public pure {
        int x;
        x = 5 + 12;
        return;
    }

    function main() public pure returns (int) {
        int x = 5 + 12;
        return x;
    }
}