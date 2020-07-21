#!/usr/bin/env bash

cd ../../../

sbt "runMain edu.cmu.cs.obsidian.Main --yul resources/tests/YulTests/EmptyContract.obs"

cd EmptyContract

solc --strict-assembly EmptyContract.yul > /dev/null

if [ $? -ne 0 ]; then
  echo "EmptyContract test failed: solc cannot compile yul code"
  exit 1
fi


rm -rf EmptyContract