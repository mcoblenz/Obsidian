#!/bin/bash

if [ -n "$1" && -n "$2" ] ; then
    peer chaincode invoke -n mycc -v 0 -C ch1 -c "{\"Function\": \"$1\", \"Args\":[$2]}"
else
    echo "Usage: $0 <function> <arguments>"
    echo "    ex.: $0 changeCarOwner '\"2\",\"Dave\"'"
    echo "       (note single quotes to prevent double quotes from being escaped by shell)"
fi
