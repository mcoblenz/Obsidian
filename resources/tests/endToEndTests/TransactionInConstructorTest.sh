#!/usr/bin/env bash

cd ../../../
# Generate java code
sbt "runMain edu.cmu.cs.obsidian.Main resources/tests/compilerTests/TransactionInConstructor.obs"
# Then go to run the tests
cd network-framework
./down.sh
./up.sh -s TransactionInConstructor

check() {
    echo "$1"
    res="$($1)"

    if [[ "$res" != "$2" ]]; then
        echo "Expected '$2' from '$1' but got '$res'"
        exit 1
    fi
}

check "./invoke.sh -q getY" "1"

./down.sh
cd ../
rm -rf TransactionInConstructor

