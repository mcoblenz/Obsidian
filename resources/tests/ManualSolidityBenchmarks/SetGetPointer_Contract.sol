// SPDX-License-Identifier: BSD

pragma solidity >=0.8.0;

contract Main {
    function main() public returns (int) {
        IntContainer d = new IntContainer();
        d.set();
        return d.get();
    }
}

contract IntContainer {
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
        return x*y*z*f; // 1800
    }

}

