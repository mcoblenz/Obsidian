#!/bin/bash

  # run_any_yul.sh takes two arguments, the path to a yul file and a
  #     decimal-represented integer, compiles that yul to evm, starts ganache,
  #     ships that evm to gananch, calls `main()`, and checks to see if the
  #     result is the same as the integer passed. this is a restricted fragment
  #     of the travis testing script but much faster and helpful for development.

if [ "$#" -ne 2 ]; then
    echo "usage: run_any_yul.sh fileOfYulCode.yul expectedValueInDecimalNotation"
    exit 1
fi


output=$(docker run -v "$( pwd -P )":/sources ethereum/solc:stable --bin --strict-assembly --optimize /sources/"$1")

TOP=$(echo "$output" | grep -n "Binary representation" | cut -f1 -d:)
BOT=$(echo "$output" | grep -n "Text representation" | cut -f1 -d:)
TOP=$((TOP+1)) # drop the line with the name
BOT=$((BOT-1)) # drop the empty line after the binary
EVM_BIN=$(echo "$output" | sed -n $TOP','$BOT'p' )
echo "binary representation is:"
echo "$EVM_BIN"

#--------------


NAME=$(basename -s '.json' "$test")
GAS=300000000000000
GAS_HEX=$(printf '%x' "$GAS")
GAS_PRICE=0x9184e72a000
START_ETH=50000000000000
NUM_ACCT=1
TESTEXP="main()"
EXPECTED="0x"$(printf '%064x' "$2")

# start up ganache
echo "starting ganache-cli"
ganache-cli --debug --gasLimit "$GAS" --accounts="$NUM_ACCT" --defaultBalanceEther="$START_ETH" &> /dev/null &

# form the JSON object to ask for the list of accounts
ACCT_DATA=$( jq -ncM \
                --arg "jn" "2.0" \
                --arg "mn" "eth_accounts" \
                --argjson "pn" "[]" \
                --arg "idn" "1" \
                '{"jsonrpc":$jn,"method":$mn,"params":$pn,"id":$idn}'
         )

# ganache-cli takes a little while to start up, and the first thing that we
# need from it is the list of accounts. so we poll on the account endpoint
# until we get a good result to avoid using sleep or something less precise.
echo "querying ganache-cli until accounts are available"
KEEPGOING=1
ACCTS=""
until [ "$KEEPGOING" -eq 0 ] ;
do
    ACCTS=$(curl --silent -X POST --data "$ACCT_DATA" http://localhost:8545)
    KEEPGOING=$?
    sleep 1
done
echo

## step 1: get an account from ganache
# todo: i'm not sure what account to use for the "to" account. (issue #302)
ACCT=$(echo "$ACCTS" | jq '.result[0]' | tr -d '"')
echo "ACCT is $ACCT"

# todo: 0x0 is the value being sent with the transaction; right now that's nothing (issue #302)
PARAMS=$( jq -ncM \
             --arg "fn" "$ACCT" \
             --arg "gn" "0x$GAS_HEX" \
	     --arg "gpn" "$GAS_PRICE" \
       	     --arg "vn" "0x0" \
 	     --arg "dn" "0x$EVM_BIN" \
             '{"from":$fn,"gas":$gn,"gasPrice":$gpn,"value":$vn,"data":$dn}')

## step 2: send a transaction
SEND_DATA=$( jq -ncM \
                --arg "jn" "2.0" \
                --arg "mn" "eth_sendTransaction" \
                --argjson "pn" "$PARAMS" \
                --arg "idn" "1" \
                '{"jsonrpc":$jn,"method":$mn,"params":$pn,"id":$idn}')

echo "transaction being sent is given by"
echo "$SEND_DATA" | jq
echo

RESP=$(curl -s -X POST --data "$SEND_DATA" http://localhost:8545)
echo "response from ganache is: " #$RESP
echo "$RESP" | jq

# todo: this is not an exhaustive check on the output from curl (issue #302)
if [ "$RESP" == "400 Bad Request" ]
then
    echo "got a 400 bad response from ganache-cli"
    exit 1
fi

ERROR=$(echo "$RESP" | tr -d '\n' | jq '.error.message')
if [ "$ERROR" != "null" ]
then
    RET=1
    echo "transaction produced an error: $ERROR"
fi

## step 3: get a transaction receipt to get the contract address
# todo: check the result of test somehow to indicate failure or not (issue #302)
# todo: this block is copied; make a function?
echo "querying ganache CLI for transaction receipt"

trans_hash=$(echo "$RESP" | jq '.result')

SEND_DATA=$( jq -ncM \
                --arg "jn" "2.0" \
                --arg "mn" "eth_getTransactionReceipt" \
                --argjson "pn" "$trans_hash" \
                --arg "idn" "1" \
                '{"jsonrpc":$jn,"method":$mn,"params":[$pn],"id":$idn}'
	 )
echo "eth_getTransactionReceipt is being sent"
echo "$SEND_DATA" | jq
echo

RESP=$(curl -s -X POST --data "$SEND_DATA" http://localhost:8545)
echo "response from ganache is: "
echo "$RESP" | jq

# check that the status code is 0x1 or else fail
if [[ ! $(echo "$RESP" | jq '.result.status' | tr -d '"' ) == "0x1" ]]
then
    echo "eth_getTransactionReceipt returned an error status; aborting"
    RET=$((RET+1))
fi


## step 4: get the contract address from the transaction receipt, stripping quotes
CONTRACT_ADDRESS=$(echo "$RESP" | jq '.result.contractAddress' | tr -d '"' )

## step 5: use call and the contract address to get the result of the function
HASH_TO_CALL=""

# the keccak implementation we want to use depends on the operating system; as of
# May 2021 i couldn't find one that was available in both apt and homebrew and produced
# output that matches the ABI, so we have to be flexible. this is a little bit of a hack.
if [[ $(uname) == "Linux" ]]
then
    # this should be what happens on Travis running Ubuntu
    if ! perl -e 'use Crypt::Digest::Keccak256 qw( :all )'
    then
        echo "the perl module Crypt::Digest::Keccak256 is not installed, Install it via cpam or 'apt install libcryptx-perl'."
        exit 1
    fi
    echo "assuming that we are on travis and getting the Keccak256 via perl"
    HASH_TO_CALL=$(echo -n "$TESTEXP" | perl -e 'use Crypt::Digest::Keccak256 qw( :all ); print(keccak256_hex(<STDIN>)."\n")' | cut -c1-8)
elif [[ $(uname) == "Darwin" ]]
then
    # this should be what happens on OS X
    if ! hash keccak-256sum
    then
        echo "keccak-256sum is not installed, Install it with 'brew install sha3sum'."
        exit 1
    fi
    echo "assuming that we are on OS X and getting the Keccak256 via keccak-256sum"
    HASH_TO_CALL=$(echo -n "$TESTEXP" | keccak-256sum | cut -d' ' -f1 | cut -c1-8)
else
    # if you are neither on travis nor OS X, you are on your own.
    echo "unable to determine OS type to pick a keccak256 implementation"
    exit 1
fi
echo "hash to call: $HASH_TO_CALL"

# "The documentation then tells to take the parameter, encode it in hex and pad it left to 32
# bytes."
PADDED_ARG=$(printf "%032g" 0)

DATA="$HASH_TO_CALL""$PADDED_ARG"

echo "padded arg: $PADDED_ARG"
echo "data: $DATA"

# build a JSON object to post to eth_call with the contract address as the to account, and
# padded data
PARAMS=$( jq -ncM \
             --arg "fn" "$ACCT" \
             --arg "tn" "$CONTRACT_ADDRESS" \
 	     --arg "dn" "0x$DATA" \
             '{"from":$fn,"to":$tn,"data":$dn}')

SEND_DATA=$( jq -ncM \
                --arg "jn" "2.0" \
                --arg "mn" "eth_call" \
                --argjson "pn" "$PARAMS" \
                --arg "idn" "1" \
                '{"jsonrpc":$jn,"method":$mn,"params":[$pn,"latest"],"id":$idn}' )
echo "eth_call is being sent"
echo "$SEND_DATA" | jq
echo

RESP=$(curl -s -X POST --data "$SEND_DATA" http://localhost:8545)
echo "response from ganache is: "
echo "$RESP" | jq

GOT=$(echo "$RESP" | jq '.result' | tr -d '"')
# todo: extend JSON object with a decode field so that we can have expected values that aren't integers more easily
if [[ "$GOT" == "$EXPECTED" ]]
then
    echo "test passed!"
else
    echo -ne "test failed! got: \n\t$GOT\nbut expected: \n\t$EXPECTED\n\n"
fi

# clean up by killing ganache and the local files
# todo: make this a subroutine that can get called at any of the exits (issue #302)
echo "killing ganache-cli"
kill -9 "$(lsof -t -i:8545)"
