// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract Return {
    function f() public pure returns (int) {
        return (4+4);
    }
    
    function main(int y) public pure returns (int) {
        return f() + y;
    }
}