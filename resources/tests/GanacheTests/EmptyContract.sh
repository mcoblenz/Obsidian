#!/usr/bin/env bash

#cd ../../../

if ! hash ganache-cli
then
    echo "ganache-cli is not installed, Install it with 'npm install -g ganache-cli'."
    exit 1
fi

if ! hash truffle
then
    echo "truffle is not installed, Install it with 'npm install -g truffle'."
    exit 1
fi


sbt "runMain edu.cmu.cs.obsidian.Main --yul resources/tests/YulTests/EmptyContract.obs"

cd EmptyContract

CURR_PATH="$( pwd -P )"

## todo: this is redundant work (and redundant code) if the other tests have run; how to use that?
docker run -v "$CURR_PATH":/sources ethereum/solc:stable --abi --bin --strict-assembly /sources/EmptyContract.yul > EmptyContract.evm 

if [ $? -ne 0 ]; then
  echo "EmptyContract test failed: solc cannot compile yul code"
  exit 1
fi

set -e

# todo: if this is in the test script for each thing, it'll spin up a new ganache-cli instance every
#  time, which is wasteful but also means that each test is insulated from each other. which do we want?

## todo: what's the right gas limit? MC: "use all the gas"


ganache-cli --gasLimit 3000 &> /dev/null &

## todo: this is a total hack 
sleep 5 # to make sure ganache-cli is up and running before compiling
rm -rf build
truffle compile
truffle migrate --reset --network development
truffle test
kill -9 $(lsof -t -i:8545)

## todo: this will run things to here, but not check the output. figure out where that ends up.

rm -rf EmptyContract
