#!/usr/bin/env bash

init() {
    cd ../../../
    # Generate java code
    if [[ ! -z "$2" ]]; then
        sbt "runMain edu.cmu.cs.obsidian.Main $2"
    else
        sbt "runMain edu.cmu.cs.obsidian.Main resources/tests/compilerTests/$1.obs"
    fi

    # Then go to run the tests
    cd network-framework

    # Check if we need to take the network down and put it back up, or if we can just upgrade.
    # This is useful when debugging; on Travis we'll always have to take it down and put it back up
    cur_proj="$( source ".env"; echo "$CHAINCODE_DIRECTORY" )"
    if [[ "$cur_proj" == "$1" ]]; then
        ./upgrade.sh
    else
        ./down.sh
        ./up.sh -s "$1"
    fi
}

check() {
    echo "$1"
    res="$($1)"

    if [[ "$res" != "$2" ]]; then
        echo "Expected '$2' from '$1' but got '$res'"
        exit 1
    fi
}

cleanup() {
    ./down.sh
    cd ../
    rm -rf "$1"
}

