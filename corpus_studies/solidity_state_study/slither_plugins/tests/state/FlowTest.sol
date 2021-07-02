// SPDX-License-Identifier: BSD-3
pragma solidity ^0.8.4;

contract Foo {
    uint mode = 0;

    function start(uint c) public view {
      uint x = mode;
      uint y = x;
      require(y == 0);
    }
}
