// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract OnOff {
  bool on = false;

  function turnOn() public {
    require(!on);
    on = true;
  }

  function turnOff() public {
    require(on);
    on = false;
  }
}

