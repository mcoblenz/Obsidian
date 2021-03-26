#!/usr/bin/env bash

#todo there's more technical debt here than just this. but. use this
#instead of the name of the file below; this will make it easier to make
#this happen in a loop real soon.
NAME=EmptyContract

# check to make sure that both tools are installed, fail otherwise.
if ! hash ganache-cli
then
    echo "ganache-cli is not installed, Install it with 'npm install -g ganache-cli'."
    exit 1
fi

# compile the contract to yul, also creating the directory to work in
sbt "runMain edu.cmu.cs.obsidian.Main --yul resources/tests/GanacheTests/$NAME.obs"

# check to make sure that solc succeeded, failing otherwise
if [ $? -ne 0 ]; then
  echo "$NAME test failed: sbt exited cannot compile obs to yul"
  exit 1
fi

#todo: check that it exists, fail otherwise.

cd $NAME

CURR_PATH="$( pwd -P )"

# generate the evm from yul
echo "running solc to produce evm bytecode"
docker run -v "$CURR_PATH":/sources ethereum/solc:stable --abi --bin --strict-assembly /sources/$NAME.yul > $NAME.evm

# check to make sure that solc succeeded, failing otherwise
if [ $? -ne 0 ]; then
  echo "$NAME test failed: solc cannot compile yul code"
  exit 1
fi

# this is a bit of a hack. solc is supposed to output a json object and it
# just isn't. so this is grepping through to grab the right lines with the
# hex that represents the output.
#
# todo: this probably fails if the binary is more than one line long.
TOP=`grep -n "Binary representation" $NAME.evm | cut -f1 -d:`
BOT=`grep -n "Text representation" $NAME.evm | cut -f1 -d:`
TOP=$((TOP+1)) #drop the line with the name
BOT=$((BOT-1)) #drop the empty line after the binary
DATA=`sed -n $TOP','$BOT'p' $NAME.evm`

echo "binary representation is: $DATA"

echo "starting ganache-cli"
# start up ganache; todo: gas is a magic number, it may be wrong. it needs
# to match what's in params below, i think. 0xbb8 is 3000.
ganache-cli --gasLimit 3000 --accounts=1 & #> /dev/null &

# todo: ping on that port or something instead of sleeping
sleep 10
echo "waking up after ganache-cli should have started, at $(pwd -P)"

echo "data is:: $DATA"

# todo there MUST be a better way to form json objects.
ACCT=`curl -X POST --data '{"jsonrpc":"2.0","method":"eth_accounts","params":[],"id":1}' 'http://localhost:8545' | jq '.result[0]'`
echo "using account $ACCT"
PARAMS='{"from":'$ACCT', "gas":"0xbb8", "gasPrice":"0x9184e72a000", "value":"0x0", "data":"0x'$DATA'"}'

echo "PARAMS is:: $PARAMS"

## nb there's a "to" field here that i'm not sure what it does but it's
## optional so i'm ignoring it. also i have no idea what the from address
## should be. i suspect it can maybe be arbitrary? the to address might be
## how i check the output.



curl -X POST --data '{"jsonrpc":"2.0","method":"eth_sendTransaction","params":'$PARAMS',"id":1}' 'http://localhost:8545'

#todo check the result of test somehow to indicate failure or not
echo "killing ganache-cli"
kill -9 $(lsof -t -i:8545)

# rm -rf $NAME #yikes
