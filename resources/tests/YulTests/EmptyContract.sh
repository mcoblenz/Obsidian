#!/usr/bin/env bash

cd ../../../

sbt "runMain edu.cmu.cs.obsidian.Main --yul resources/tests/YulTests/EmptyContract.obs"

cd EmptyContract

solc --strict-assembly EmptyContract.yul

rm -rf EmptyContract