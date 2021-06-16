// SPDX-License-Identifier: BSD-3
pragma solidity ^0.8.4;

contract A {
    enum Stage {NotStarted, InProgress, Ended}
    Stage stage;

    function getStage() public view returns (Stage) {
      return stage;
    }

    modifier inStage(Stage _stage) {
      require(getStage() == _stage);
      _;
    }

    modifier blah(uint x, Stage _stage) {
      require(stage == Stage.Ended);
      _;
    }
    
    function startGame() public inStage(Stage.NotStarted) {
      stage = Stage.InProgress;
    }
}
