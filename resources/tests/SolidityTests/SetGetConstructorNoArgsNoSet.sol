// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract SetGetConstructorNoArgsNoSet {
    
    int x;

    constructor() {
        x = 0;
    }

    function set(int value) public {
        x = value;
    }

    function get() public view returns (int) {
        return x;
    }

    function main() public view returns (int) {
        return (get());
    }
}