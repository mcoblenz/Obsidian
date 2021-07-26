// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract BoolLiteral {
    function BoolLit() public pure {
        bool x;
        x = true;
        x = false;
    }
    
    function main() public pure returns (bool) {
        return false;
    }
}