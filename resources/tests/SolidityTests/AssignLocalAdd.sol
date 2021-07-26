// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract AssignLocalAdd {
    function AssignLAdd() public pure {
        int x;
        bool y;
        x = 5 + 12;
        return;
    }
    
    function main() public pure returns (int) {
        int x = 5 + 12;
        return x;
    }
}