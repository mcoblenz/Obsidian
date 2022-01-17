// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract SetGetNoArgsNoConstructNoInit {
    
    int x;

    function set() public {
        x = 5;
    }

    function get() public view returns (int) {
        return x;
    }

    function main() public returns (int) {
        set();
        return (get());
    }
}