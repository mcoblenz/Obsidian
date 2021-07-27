// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract IntConst {
    function intconst() public pure {
        int x;
        x = 12;
        return;
    }
    
    function main() public pure returns (int) {
        return 12;
    }
}