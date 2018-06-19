#!/bin/bash

if [ -n "$1" ] ; then
    peer chaincode invoke -n mycc -v 0 -C ch1 -c "$1"
fi
