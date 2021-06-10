#!/bin/bash

# travis makes this env var available to all builds, so this stops us from installing things locally
if [[ $CI == "true" ]]
then
  ./travis_specific/install_ganache.sh
fi

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

missing_json=$(diff --changed-group-format='%<%>' --unchanged-group-format='' <(ls resources/tests/GanacheTests/*.json | xargs basename -s '.json') <(ls resources/tests/GanacheTests/*.obs | xargs basename -s '.obs'))
if [ "$missing_json" ]
then
  echo "******** warning: some tests are defined but do not have json files and will not be run:"
  echo "$missing_json"
fi
# keep track of which tests fail so that we can output that at the bottom of the log
failed=()

# build a jar of Obsidian but removing all the tests; that happens in the other Travis matrix job, so we can assume it works here.
sbt 'set assembly / test := {}' ++$TRAVIS_SCALA_VERSION assembly

# check that the jar file for obsidian exists; `sbt assembly` ought to have been run before this script gets run
obsidian_jar="$(find target/scala* -name obsidianc.jar | head -n1)"
if [[ ! "$obsidian_jar" ]]
then
        echo "Error building Obsidian jar file, exiting."
        exit 1
fi

for test in "${tests[@]}"
do
  echo "---------------------------------------------------------------"
  echo "running Ganache Test $test"
  echo "---------------------------------------------------------------"

  # pull values out of the json file for the test; some of these are integers stored as hex strings
  # because they're so large, which means we have to cut out the quotation marks.
  NAME=$(basename -s '.json' "$test")
  GAS=$(<"$test" jq '.gas')
  GAS_HEX=$(printf '%x' "$GAS")
  GAS_PRICE=$(<"$test" jq '.gasprice' | tr -d '"')
  START_ETH=$(<"$test" jq '.startingeth')
  NUM_ACCT=$(<"$test" jq '.numaccts')
  TESTEXP=$(<"$test" jq '.testexp' | tr -d '"')
  EXPECTED=$(<"$test" jq '.expected' | tr -d '"')

  CHECK_OUTPUT=true

  if [[ $TESTEXP == "null" ]]
  then
    echo "*****WARNING: no test expression supplied"
    CHECK_OUTPUT=false
  fi

  if [[ $EXPECTED == "null" ]]
  then
    echo "*****WARNING: no expected result supplied"
    CHECK_OUTPUT=false
  fi

  # compile the contract to yul, also creating the directory to work in, failing otherwise
  if ! $(java -jar $obsidian_jar --yul resources/tests/GanacheTests/$NAME.obs)
  then
      echo "$NAME test failed: cannot compile obs to yul"
      failed+=("$test [compile obs to yul]")
      exit 1
  fi

  if [ ! -d "$NAME" ]; then
      echo "$NAME directory failed to get created"
      failed+=("$test [output directory]")
      exit 1
  fi

  cd "$NAME" || exit 1

  # generate the evm from yul, failing if not
  echo "running solc to produce evm bytecode"
  if ! docker run -v "$( pwd -P )":/sources ethereum/solc:stable --abi --bin --strict-assembly /sources/"$NAME".yul > "$NAME".evm
  then
      echo "$NAME test failed: solc cannot compile yul code"
      failed+=("$test [solc]")
      exit 1
  fi

  # grep through the format output by solc in yul producing mode for the binary representation
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
      failed+=("$test [400]")
      RET=$((RET+1))
      continue
  fi

  ERROR=$(echo "$RESP" | tr -d '\n' | jq '.error.message')
  if [ "$ERROR" != "null" ]
  then
      RET=$((RET+1))
      echo "transaction produced an error: $ERROR"
      failed+=("$test [transaction]")
      continue
  fi

  if [ $CHECK_OUTPUT == "true" ]
  then
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
      failed+=("$test [eth_getTransactionReceipt]")
      continue
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
          failed+=("$test [perl hash]")
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
          failed+=("$test [os x hash]")
          exit 1
        fi
        echo "assuming that we are on OS X and getting the Keccak256 via keccak-256sum"
        HASH_TO_CALL=$(echo -n "$TESTEXP" | keccak-256sum | cut -d' ' -f1 | cut -c1-8)
    else
        # if you are neither on travis nor OS X, you are on your own.
        echo "unable to determine OS type to pick a keccak256 implementation"
        failed+=("$test [no hash]")
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

    ERROR=$(echo "$RESP" | tr -d '\n' | jq '.error.message')
    if [ "$ERROR" != "null" ]
    then
        RET=$((RET+1))
        echo "eth_call returned an error: $ERROR"
        failed+=("$test [eth_call]")
        continue
    fi

    # pull the result out of the JSON object, delete the quotes and leading 0x, and make it upper case
    GOT=$(echo "$RESP" | jq '.result' | tr -d '"' | sed -e "s/^0x//" | tr '[:lower:]' '[:upper:]')
    # use BC to convert it to decimal
    GOT_DEC=$(echo "obase=10; ibase=16;$GOT" | bc)

    # todo: extend JSON object with a decode field so that we can have expected values that aren't integers more easily
    if [ "$GOT_DEC" == "$EXPECTED" ]
    then
      echo "expected $EXPECTED (in decimal)"
      echo "test passed!"
    else
      echo "test failed! got $GOT_DEC but expected $EXPECTED"
      RET=$((RET+1))
      failed+=("$test [wrong answer]")
    fi
  else
    echo "*****WARNING: not checking the output of running this code because the JSON describing the test didn't include it"
  fi

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

echo
echo "----------------------------"
echo "ganache test quick summary:"
echo "----------------------------"
if [ ${#failed[@]} -ne 0 ]
then
  echo "failed tests:"
  for failure in "${failed[@]}"
  do
    echo "$failure"
  done
else
  echo "no failed tests!"
fi

exit "$ANY_FAILURES"
