// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract MultiLineIfRetSm {
    function val() public pure returns (int) {
        int x = 0;
        if (true && ! false) {
            x = 20;
            return 4 + x;
        }
        else {
            return 13;
        }
    }
    
    function main() public pure returns (int) {
        int x;
        x = 9 + 0;
        x = val();
        return x;
    }
}