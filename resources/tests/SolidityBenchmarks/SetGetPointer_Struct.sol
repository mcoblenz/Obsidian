// SPDX-License-Identifier: BSD

pragma solidity >=0.8.0;

contract IntContainer {

    struct Data {
        int x;
        int y;
        int z;
        int f;
    }

    function set(Data memory d) pure public {
        d.x = -1;
        d.f = -1;
        d.z = -1;
        d.y = -1;
        d.x = 5;
        d.y = 10;
        d.z = 4;
        d.f = 9;
    }

    function get(Data memory d) pure public returns (int) {
        return d.x*d.y*d.z*d.f; // 1800
    }

    function main() public pure returns (int) {
        Data memory d;
        set(d);
        return get(d);
    }


}