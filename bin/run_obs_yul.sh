#!/bin/bash

if [ ! $(basename $(pwd -P)) == "Obsidian" ]
then
    echo "don't run this unless you're in the obsidian tld"
    exit 1
fi


DIR="resources/tests/GanacheTests"

if [ ! "$1" ]
then
    echo "provide an argument! your options are:"
    ls -1 "$DIR"/*.obs | xargs basename -s ".obs"
    exit 1
fi

TEST="$DIR/$1.obs"

if [ ! -e "$TEST" ]
then
    echo "$TEST does not exist"
    exit 1
fi

if [ -d "$1" ]
then
   rm -f "$1/$1.yul"
   rmdir "$1"
fi

sbt "runMain edu.cmu.cs.obsidian.Main --yul $TEST" || exit 1

SOLC_OUT=$(docker run -v "$( pwd -P )/$1":/sources ethereum/solc:stable --strict-assembly "/sources/$1.yul")

TOP=$(echo "$SOLC_OUT" | grep -n "Pretty printed source:" | cut -f1 -d:)
BOT=$(echo "$SOLC_OUT" | grep -n "Binary representation:" | cut -f1 -d:)
TOP=$((TOP+1))
BOT=$((BOT-1))

echo "*********"
echo "THIS OUTPUT IS PRETTY-PRINTED BY SOLC, IT IS NOT LITERALLY WHAT WE PRODUCE"
echo "*********"

echo "$SOLC_OUT" | sed -n $TOP','$BOT'p' | sed '/^[[:space:]]*$/d'
