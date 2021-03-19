#!/usr/bin/env bash

# check to make sure that both tools are installed, fail otherwise.
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

# compile the contract to yul, also creating the directory to work in
sbt "runMain edu.cmu.cs.obsidian.Main --yul resources/tests/GanacheTests/EmptyContract.obs"

# copy
cp resources/tests/GanacheTests/truffle-config.js EmptyContract/
cd EmptyContract
truffle init --force

CURR_PATH="$( pwd -P )"

## todo: this is redundant work (and redundant code) if the other tests
## have run; how to use that?
docker run -v "$CURR_PATH":/sources ethereum/solc:stable --abi --bin --strict-assembly EmptyContract.yul > contracts/EmptyContract.evm

if [ $? -ne 0 ]; then
  echo "EmptyContract test failed: solc cannot compile yul code"
  exit 1
fi

set -e

ganache-cli --gasLimit 3000 &> /dev/null &

## todo: this is a total hack
sleep 5 # to make sure ganache-cli is up and running before compiling
echo "waking up after ganache-cli should have started, at $(pwd -P)"

rm -rf build
truffle compile
truffle migrate --reset --network development
truffle test
kill -9 $(lsof -t -i:8545)

## todo: this will run things to here, but not check the output. figure out where that ends up.

rm -rf EmptyContract
