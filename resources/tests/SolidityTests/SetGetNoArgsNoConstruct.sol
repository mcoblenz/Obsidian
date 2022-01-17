// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract SetGetNoArgsNoConstruct {
    
    int x;

    function set1() public {
        x = 5;
    }

    function set2() public {
        x = 10;
    }

    function get() public view returns (int) {
        return x;
    }

    function main() public returns (int) {
        set1();
        set2();
        return (get());
    }
}