#!/bin/bash

printHelp() {
    echo
    echo "You can make a single function invocation with any number of arguments"
    echo "The format is ./invoke.sh [-q] [function name] [arg1] [arg2] ..."
    echo "-q will only print the payload, so one can store the payload in some variable"
    echo "To print this info again, do ./invoke.sh -h or ./invoke.sh -?"
    echo
}

if [ $# = 0 ]
then
    printHelp
    exit 0
fi

while getopts "h?" opt; do
  case "$opt" in
  h | \?)
    printHelp
    exit 0
    ;;
   esac
done

confirmNetworkUp() {
    CLI=$(docker ps -f name=cli -q)
    if [ -z "$CLI" ]; then
        echo "Fabric network not found. Did you remember to run up.sh?"
        exit 0
    fi
}

confirmNetworkUp
docker exec cli scripts/invoke.sh $@