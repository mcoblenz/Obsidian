// SPDX-License-Identifier: BSD-3

pragma solidity >=0.4.22 <0.9.0;

contract Pausable {
        event Pause();
        event Unpause();
        bool public paused = false;
        modifier whenNotPaused() {
            require(!paused);
            _;
        }
        modifier whenPaused {
            require(paused);
            _;
        }
        function pause() public whenNotPaused returns (bool) {
            paused = true;
            emit Pause();
            return true;
        }
        function unpause() public whenPaused returns (bool) {
            paused = false;
            emit Unpause();
            return true;
        }
    }
