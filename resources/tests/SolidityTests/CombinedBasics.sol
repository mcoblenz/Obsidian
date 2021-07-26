// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract CombinedBasics {
    function val() public pure returns (int) {
        int x = 70;
        if ( true && ! false) {
            x = 4 + 20;
        }
        else {
            x = 13;
        }
        return x;
    }
    
    function main() public pure returns (int) {
        int x;
        x = 9 + 0;
        x = val();
        return x;
    }
}