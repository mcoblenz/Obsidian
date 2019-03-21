#!/usr/bin/env bash

printHelp() {
    echo
    echo "Usage: "
    echo -n 'temp=`./getGuid.sh` '
    echo "will load the guid returned from the last invocation to temp"
    echo "the last invocation must construct or return an object"
    echo
}

if [ $# -ne 0 ]
then
    printHelp
    exit 0
fi

docker exec cli scripts/getGuid.sh