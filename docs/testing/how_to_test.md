# Testing

This file describes how to use Travis CI to compile Obsidian programs to YUL, build that YUL code with Truffle, run the result on a blockchain provided to the Travis container via Ganache, specify a desired result and compare the output to that specification, and pass or fail CI based on this comparison.

## Resources
* https://blog.coinfabrik.com/test-solidity-smart-contracts-using-travis-ci/
  
* https://medium.com/haloblock/deploy-your-own-smart-contract-with-truffle-and-ganache-cli-beginner-tutorial-c46bce0bd01e

* https://github.com/trufflesuite/ganache-cli/