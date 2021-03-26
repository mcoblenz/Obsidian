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

# check to make sure that solc succeeded, failing otherwise
if [ $? -ne 0 ]; then
  echo "EmptyContract test failed: sbt exited cannot compile obs to yul"
  exit 1
fi

# copy in the cached output of truffle init
cp -r resources/tests/GanacheTests/truffle-env/* EmptyContract/

# copy in the js describing the tests; todo: does this have to be named test.js?
cp resources/tests/GanacheTests/EmptyContract.test.js EmptyContract/test/test.js

cd EmptyContract

CURR_PATH="$( pwd -P )"

# generate the evm from yul
docker run -v "$CURR_PATH":/sources ethereum/solc:stable --abi --bin --strict-assembly /sources/EmptyContract.yul > contracts/EmptyContract.evm

# check to make sure that solc succeeded, failing otherwise
if [ $? -ne 0 ]; then
  echo "EmptyContract test failed: solc cannot compile yul code"
  exit 1
fi

# todo: this exits if there was a non-zero exit status but the above if
# statement should have the same semantics? i don't know why this is here,
# maybe it can be deleted.
set -e

# start up ganache; todo: gas is a magic number, it may be wrong
ganache-cli --gasLimit 3000 &> /dev/null &

## todo: this is a total hack
sleep 5 # to make sure ganache-cli is up and running before compiling
echo "waking up after ganache-cli should have started, at $(pwd -P)"

rm -rf build
truffle compile
truffle migrate --reset --network development
truffle test

#todo check the result of truffle test somehow to indicate failure or not

kill -9 $(lsof -t -i:8545)

rm -rf EmptyContract
