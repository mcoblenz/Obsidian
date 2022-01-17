// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract SetGetTwoConstructors {
    
    int x;

    constructor(int a, int b) {
        x = a + b;
    }

    function set(int value) public {
        x = value;
    }

    function get() public view returns (int) {
        return x;
    }

    function main() public returns (int) {
        int y = get();
        set(y+5);
        return (get());
    }
}