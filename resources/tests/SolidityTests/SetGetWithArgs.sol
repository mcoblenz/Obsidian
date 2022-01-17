// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.18 <0.9.0;

contract SetGetWithArgs {
    
    int x;

    function set(int value) public {
        x = value;
    }

    function get() public view returns (int) {
        return x;
    }

    function main() public returns (int) {
        set(5);
        return (get());
    }
}