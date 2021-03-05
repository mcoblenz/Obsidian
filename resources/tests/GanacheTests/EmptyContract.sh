#!/usr/bin/env bash

#cd ../../../

sbt "runMain edu.cmu.cs.obsidian.Main --yul resources/tests/YulTests/EmptyContract.obs"

cd EmptyContract

CURR_PATH="$( pwd -P )"

## todo: this is redundant work (and redundant code) if the other tests have run; how to use that?
docker run -v "$CURR_PATH":/sources ethereum/solc:stable --abi --bin --strict-assembly /sources/EmptyContract.yul > /dev/null

if [ $? -ne 0 ]; then
  echo "EmptyContract test failed: solc cannot compile yul code"
  exit 1
fi

set -e

# todo: if this is in the test script for each thing, it'll spin up a new ganache-cli instance every
#  time, which is wasteful but also means that each test is insulated from each other. which do we want?

## todo: what's the right gas limit?
ganache-cli --gasLimit 3000 2> /dev/null 1> /dev/null &
sleep 5 # to make sure ganache-cli is up and running before compiling ## todo: this is from the doc i scraped but dear god is it a hack and i hate it
rm -rf build
truffle compile
truffle migrate --reset --network development
truffle test
kill -9 $(lsof -t -i:8545)

## todo: this will run things to here, but not check the output. figure out where that ends up.

rm -rf EmptyContract