// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract CombinedBasicsRet {
    function val() public pure returns (int) {
        if ( true && ! false) {
            return 4 + 20;
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