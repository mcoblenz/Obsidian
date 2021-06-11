// SPDX-License-Identifier: BSD-3
pragma solidity ^0.8.4;

contract A {
    enum Stage {NotStarted, InProgress, Ended}
    Stage stage;

    //This function is currently not flagged as having state.
    //Even though this modifier takes a parameter, this doesn't necessarily mean
    //that it depends on function parameters--we need to look at how the modifier
    //is used. In the function startGame, the enum value Stage.NotStarted is being
    //passed into this modifier.
    modifier inStage(Stage _stage) {
      require(stage == _stage);
      _;
    }

    modifier blah() {
      require(stage == Stage.Ended);
      _;
    }

    function startGame() public inStage(Stage.NotStarted) blah {
      stage = Stage.InProgress;
    }
}
