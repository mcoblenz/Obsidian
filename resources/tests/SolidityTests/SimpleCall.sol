// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract SimpleCall {
    function val() public pure returns (int) {
        return 4;
    }
    
    function main() public pure returns (int) {
        int x;
        x = 9;
        x = val();
        return x;
    }
}