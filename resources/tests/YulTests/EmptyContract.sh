#!/usr/bin/env bash

#cd ../../../

sbt "runMain edu.cmu.cs.obsidian.Main --yul resources/tests/YulTests/EmptyContract.obs"

cd EmptyContract

CURR_PATH="$( pwd -P )"

docker run -v "$CURR_PATH":/sources ethereum/solc:stable --abi --bin --strict-assembly /sources/EmptyContract.yul > /dev/null

if [ $? -ne 0 ]; then
  echo "EmptyContract test failed: solc cannot compile yul code"
  exit 1
fi


# this isn't getting run and ends up in the TLD
rm -rf EmptyContract