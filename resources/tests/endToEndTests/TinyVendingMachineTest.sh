#!/usr/bin/env bash

cd ../../../
# Generate java code
sbt "runMain edu.cmu.cs.obsidian.Main resources/demos/TinyVendingMachine/TinyVendingMachine.obs"
# Then go to run the tests
cd network-framework
./up.sh -s TinyVendingMachine

CANDY=$(./instantiateOther.sh -q Candy)

if [[ $? != 0 ]]; then
    echo "Instantiating Candy failed."
    exit 1
fi

./invoke.sh -q restock -g CandyOrGUID $CANDY
if [[ $? != 0 ]]; then
    echo "Invoking restock failed."
    exit 1
fi

COIN=$(./instantiateOther.sh -q Coin)
if [[ $? != 0 ]]; then
    echo "Instantiating Coin failed."
    exit 1
fi

check() {
    echo "$1"
    res="$($1)"

    if [[ "$res" != "$2" ]]; then
        echo "Expected '$2' from '$1' but got '$res'"
        exit 1
    fi
}

check "./invoke.sh -q buy -g CoinOrGUID $COIN" "$CANDY"

./down.sh
cd ..
rm -rf TinyVendingMachine

