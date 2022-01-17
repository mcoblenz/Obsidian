// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract SetGetMainArgs3 {
    
    int x;

    constructor(int fc1, int fc2, int fc3) {
        x = fc1 + fc2 + fc3;
    }

    function set(int value) public {
        x = value;
    }

    function get() public view returns (int) {
        return x;
    }

    function main() public view returns (int) {
        return (get() + 2);
    }
}