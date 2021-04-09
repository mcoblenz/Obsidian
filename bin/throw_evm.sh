#!/bin/bash

GAS=30000000                  # this is a magic number (issue #302)
GAS_HEX=$(printf '%x' $GAS)
GAS_PRICE='0x9184e72a000'     # this is a magic number from the YUL docs (issue #302)
START_ETH=5000000             # this is a magic number (issue #302)
NUM_ACCT=1

# start up ganache
echo "starting ganache-cli"
ganache-cli --gasLimit "$GAS" --accounts="$NUM_ACCT" --defaultBalanceEther="$START_ETH" &> /dev/null &

# form the JSON object to ask for the list of accounts
ACCT_DATA=$( jq -ncM \
                --arg "jn" "2.0" \
                --arg "mn" "eth_accounts" \
                --argjson "pn" "[]" \
                --arg "idn" "1" \
                '{"jsonrpc":$jn,"method":$mn,"params":$pn,"id":$idn}'
         )

# we'll keep querying for the accounts until we get a good result, this
# will also block until ganache-cli comes up.
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

# todo: i'm not sure what account to mark as the "to" account. i think i
# can use that later to test the output of running more complicated
# contracts. i'll need to make more than one account when i start up
# ganache. (issue #302)
ACCT=`echo $ACCTS | jq '.result[0]' | tr -d '"'`
echo "ACCT is $ACCT"

# todo what's that 0x0 mean?
PARAMS=$( jq -ncM \
             --arg "fn" "$ACCT" \
             --arg "gn" "0x$GAS_HEX" \
	     --arg "gpn" "$GAS_PRICE" \
	     --arg "vn" "0x0" \
	     --arg "dn" "0x$1" \
             '{"from":$fn,"gas":$gn,"gasPrice":$gpn,"value":$vn,"data":$dn}'
      )

SEND_DATA=$( jq -ncM \
                --arg "jn" "2.0" \
                --arg "mn" "eth_sendTransaction" \
                --argjson "pn" "$PARAMS" \
                --arg "idn" "1" \
                '{"jsonrpc":$jn,"method":$mn,"params":$pn,"id":$idn}'
      )

echo "transaction being sent is given by"
echo "$SEND_DATA" | jq
echo

RESP=$(curl -s -X POST --data "$SEND_DATA" http://localhost:8545)
echo "response from ganache is"
((echo "$RESP" | tr -d '\n') ; echo) | jq # (issue #302)

echo "killing ganache-cli"
kill -9 $(lsof -t -i:8545)
