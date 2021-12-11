// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract MultiLineIfRet {
    function val() public pure returns (int) {
        int x = 0;
        if (true && ! false) {
            x = 4+20;
            x = 11;
            return 4 + x;
        }
        else {
            x = 13;
            x = 90;
            return 13;
        }
    }
    
    function main(int y) public pure returns (int) {
        int x;
        x = 9 + 0;
        x = val() + y;
        return x;
    }
}