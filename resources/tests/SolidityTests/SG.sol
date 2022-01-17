// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract SG {
    int x;
    int y;
    int z;
    int f;

    function set() public {
        x = -1;
        f = -1;
        z = -1;
        y = -1;
        x = 5;
        y = 10;
        z = 4;
        f = 9;
    }

    function get() public view returns (int) {
        return x*y*z*f;
    }

    function main() public returns (int) {
        set();
        return get();
    }
}