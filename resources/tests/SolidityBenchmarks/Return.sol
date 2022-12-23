// SPDX-License-Identifier: BSD

pragma solidity >=0.8.0;


contract Return {
    function f() public pure returns (int) {
        return (4+4);
    }
    function g() pure public {
        int x = f();
        return;
    }

    function main() public returns (int) {
        return f();
    }
}
