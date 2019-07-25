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
    ./down.sh
    ./up.sh -s "$1"
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

