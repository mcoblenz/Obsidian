#!/bin/bash -x

#todo there's more technical debt here than just this. but. use this
#instead of the name of the file below; this will make it easier to make
#this happen in a loop real soon.
NAME=EmptyContract
GAS=30000000
GAS_HEX=$(printf '%x' $GAS) # 0x1C9C380

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

if [ ! -d "$NAME" ]; then
    echo "$NAME directory failed to get created"
    exit 1
fi

cd "$NAME"

# generate the evm from yul
echo "running solc to produce evm bytecode"
docker run -v "$( pwd -P )":/sources ethereum/solc:stable --abi --bin --strict-assembly /sources/$NAME.yul > $NAME.evm

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
EVM_BIN=`sed -n $TOP','$BOT'p' $NAME.evm`

echo "binary representation is: $EVM_BIN"

echo "starting ganache-cli"
# start up ganache; todo: gas is a magic number, it may be wrong. it needs
# to match what's in params below, i think. 0xbb8 is 3000.
ganache-cli --gasLimit $GAS --accounts=1 --defaultBalanceEther=5000000 & #> /dev/null &

#form the JSON object to ask for the list of accounts
ACCT_DATA=$( jq -ncM \
                --arg "jn" "2.0" \
                --arg "mn" "eth_accounts" \
                --argjson "pn" "[]" \
                --arg "idn" "1" \
                '{"jsonrpc":$jn,"method":$mn,"params":$pn,"id":$idn}'
         )

#we'll keep querying for the accounts until we get a good result, this will
#also block until ganache-cli comes up.
echo "querying ganache-cli until accounts are available"
KEEPGOING=1
ACCTS=""
until [ "$KEEPGOING" -eq 0 ] ;
do
    ACCTS=$(curl --silent -X POST --data "$ACCT_DATA" http://localhost:8545)
    KEEPGOING=$?
    printf '.'
    sleep 1
done
echo


echo "waking up after ganache-cli should have started, at $(pwd -P)"

# we'll return this at the exit at the bottom of the file; TravisCI says a
# job passes or fails based on the last command run
RET=0

# todo there MUST be a better way to form json objects.

## nb there's a "to" field here that i'm not sure what it does but it's
## optional so i'm ignoring it. also i have no idea what the from address
## should be. i suspect it can maybe be arbitrary? the to address might be
## how i check the output.

ACCT=`echo $ACCTS | jq '.result[0]' | tr -d '"'`
echo "ACCT is $ACCT"


######## START HERE
PARAMS=$( jq -ncM \
             --arg "fn" "$ACCT" \
             --arg "gn" "0x$GAS_HEX" \
	     --arg "gpn" "0x9184e72a000" \
	     --arg "vn" "0x0" \
	     --arg "dn" "0x$EVM_BIN" \
             '{"from":$fn,"gas":$gn,"gasPrice":$gpn,"value":$vn,"data":$dn}'
      )

SEND_DATA=$( jq -ncM \
                --arg "jn" "2.0" \
                --arg "mn" "eth_sendTransaction" \
                --argjson "pn" "$PARAMS" \
                --arg "idn" "1" \
                '{"jsonrpc":$jn,"method":$mn,"params":$pn,"id":$idn}'
      )

#PARAMS='{"from":'$ACCT', "gas":"'$GAS_HEX'", "gasPrice":"0x9184e72a000", "value":"0x0", "data":"0x'$DATA'"}'
echo "PARAMS is $PARAMS"
#RESP=`curl -s -X POST --data '{"jsonrpc":"2.0","method":"eth_sendTransaction","params":'$PARAMS',"id":1}' 'http://localhost:8545'`
RESP=$(curl -s -X POST --data "$SEND_DATA" http://localhost:8545)
echo "response from ganache is: $RESP"

if [ "$RESP" == "400 Bad Request" ]
then
    echo "got a 400 bad response from ganache-cli"
    exit 1
fi

ERROR=$(echo "$RESP" | tr -d '\n' | jq '.error.message')
if [[ $ERROR -ne "null" ]]
then
    RET=1
    echo "transaction produced an error: $ERROR"
fi

#todo check the result of test somehow to indicate failure or not


# clean up; todo: make this a subroutine that can get called at any of the exits
echo "killing ganache-cli"
kill -9 $(lsof -t -i:8545)

rm "$NAME.yul"
rm "$NAME.evm"
cd "../"
rmdir "$NAME"

exit $RET
