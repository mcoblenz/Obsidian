#!/bin/bash

if [ -n "$1" ] ; then
    if [ "$1" == "-" ] ; then
        peer chaincode instantiate -n mycc -v 0 -C ch1 -l java -c "{\"Args\": [\"init\"]}"
    else
        peer chaincode instantiate -n mycc -v 0 -C ch1 -l java -c "{\"Args\": [\"init\",$1]}"
    fi
else
    echo "Usage: $0 <arguments>"
    echo "    ex.: $0 '\"2\",\"Dave\"'"
    echo "       (note single quotes to prevent double quotes from being escaped by shell)"
    echo "To instantiate with no arguments, use: $0 -"
fi
