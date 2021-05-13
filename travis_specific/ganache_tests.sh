#!/bin/bash

# note: this won't be set locally so either set it on your machine to make
# sense or run this only via travis.
cd "$TRAVIS_BUILD_DIR" || exit 1

ANY_FAILURES=0

# check to make sure that ganache is installed, fail otherwise.
if ! hash ganache-cli
then
    echo "ganache-cli is not installed, Install it with 'npm install -g ganache-cli'."
    exit 1
fi

# either test only the directories named as arguments or test everything if nothing is specified.
# since the travis.yml file doesn't give any argument here, CI will test everything, but this makes
# local testing easier.
tests=()
if [ $# -gt 0 ]
then
  for arg in "$@"
  do
    tests+=("resources/tests/GanacheTests/$arg.json")
  done
else
  tests=(resources/tests/GanacheTests/*.json)
fi

for test in "${tests[@]}"
do
  echo "---------------------------------------------------------------"
  echo "running Ganache Test $test"
  echo "---------------------------------------------------------------"

  NAME=$(basename -s '.json' "$test")
  GAS=$(<"$test" jq '.gas')
  GAS_HEX=$(printf '%x' "$GAS")
  # nb: we store gas price as a string because it's usually quite large so
  # it's good to have it in hex notation, but that means we need to crop
  # off the quotations.
  GAS_PRICE=$(<"$test" jq '.gasprice' | tr -d '"')
  START_ETH=$(<"$test" jq '.startingeth')
  NUM_ACCT=$(<"$test" jq '.numaccts')
  TESTEXP=$(<"$test" jq '.testexp' | tr -d '"')
  EXPECTED=$(<"$test" jq '.expected' | tr -d '"')

  # compile the contract to yul, also creating the directory to work in, failing otherwise
  if ! sbt "runMain edu.cmu.cs.obsidian.Main --yul resources/tests/GanacheTests/$NAME.obs"
  then
      echo "$NAME test failed: sbt exited cannot compile obs to yul"
      exit 1
  fi

  if [ ! -d "$NAME" ]; then
      echo "$NAME directory failed to get created"
      exit 1
  fi

  cd "$NAME" || exit 1

  # generate the evm from yul, failing if not
  echo "running solc to produce evm bytecode"
  if ! docker run -v "$( pwd -P )":/sources ethereum/solc:stable --abi --bin --strict-assembly /sources/"$NAME".yul > "$NAME".evm
  then
      echo "$NAME test failed: solc cannot compile yul code"
      exit 1
  fi

  # todo: this is a bit of a hack. solc is supposed to output a json object
  # and it just isn't. so this is grepping through to grab the right lines
  # with the hex that represents the output. (issue #302)
  TOP=$(grep -n "Binary representation" "$NAME".evm | cut -f1 -d:)
  BOT=$(grep -n "Text representation" "$NAME".evm | cut -f1 -d:)
  TOP=$((TOP+1)) # drop the line with the name
  BOT=$((BOT-1)) # drop the empty line after the binary
  EVM_BIN=$(sed -n $TOP','$BOT'p' "$NAME".evm)
  echo "binary representation is: $EVM_BIN"

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

  # we'll return this at the exit at the bottom of the file; TravisCI says a
  # job passes or fails based on the last command run
  RET=0

  ## step 1: get an account from ganache
  # todo: i'm not sure what account to use for the "to" account. (issue #302)
  ACCT=$(echo "$ACCTS" | jq '.result[0]' | tr -d '"')
  echo "ACCT is $ACCT"

  # todo: what's that 0x0 mean?
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

  ## step 3: get a transaction receipt
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

  ## step 4: get the contract address from the transaction receipt
  CONTRACT_ADDRESS=$(echo "$RESP" | jq '.result.contractAddress')

  ## step 5: use call and the contract address to get the result of the function
  # todo: right now this is hard coded for simple call; in the future, each test will have a
  # file with the expression we want to run and i'll write a script that encodes it
  HASH_TO_CALL=""

  if [[ $(uname) == "Linux" ]]
  then
      # this should be what happens on Travis running Ubuntu
      if ! $(perl -e 'use Crypt::Digest::Keccak256 qw( :all )')
      then
        echo "the perl module Crypt::Digest::Keccak256 is not installed, Install it via cpam or 'apt install libcryptx-perl'."
        exit 1
      fi
      HASH_TO_CALL=$(echo -n "$TESTEXP" | perl -e 'use Crypt::Digest::Keccak256 qw( :all ); print(keccak256_hex(<STDIN>)."\n")')
  elif [[ $(uname) == "Darwin" ]]
  then
      # this should be what happens on OS X
      if ! hash keccak-256sum
      then
        echo "keccak-256sum is not installed, Install it with 'brew install sha3sum'."
        exit 1
      fi
      HASH_TO_CALL=$(echo -n "$TESTEXP" | keccak-256sum | cut -d' ' -f1 | cut -c1-8)
  else
      # if you are neither on travis nor OS X, you are on your own.
      echo "unable to determine OS type to pick a keccak256 implementation"
      exit 1
  fi
  echo "hash to call: $HASH_TO_CALL"

  # "The documentation then tells to take the parameter, encode it in hex and pad it left to 32
  # bytes. Which would be the following, using the number "5":"
  PADDED_ARG=$(printf "%032g" 0)

  DATA=0x"$HASH_TO_CALL""$PADDED_ARG"

  echo "padded arg: $PADDED_ARG"
  echo "data: $DATA"

  PARAMS=$( jq -ncM \
               --arg "fn" "$ACCT" \
               --arg "tn" "$ACCT" \
 	             --arg "dn" "0x$DATA" \
               '{"from":$fn,"to":$tn,"data":$dn,"latest"}')

  SEND_DATA=$( jq -ncM \
                  --arg "jn" "2.0" \
                  --arg "mn" "eth_call" \
                  --argjson "pn" "$PARAMS" \
                  --arg "idn" "1" \
                  '{"jsonrpc":$jn,"method":$mn,"params":[$pn],"id":$idn}'
	)
  echo "eth_call is being sent"
  echo "$SEND_DATA" | jq
  echo

  RESP=$(curl -s -X POST --data "$SEND_DATA" http://localhost:8545)
  echo "response from ganache is: "
  echo "$RESP" | jq

  # todo: compare expected to what we got!

  # clean up by killing ganache and the local files
  # todo: make this a subroutine that can get called at any of the exits (issue #302)
  echo "killing ganache-cli"
  kill -9 "$(lsof -t -i:8545)"

  # todo: for debugging it's nice to be able to look at these. maybe delete
  # them by default but take a flag to keep them around. (issue #302)
  rm "$NAME.yul"
  rm "$NAME.evm"
  cd "../"
  rmdir "$NAME"

  if [ $RET -ne 0 ]
  then
      ANY_FAILURES=1
  fi
done

exit "$ANY_FAILURES"
