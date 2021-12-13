#!/bin/bash

# kill stray ganache cli instances by scanning that port

if [ "$(lsof -t -i:8545)" ]
then
       kill -9 "$(lsof -t -i:8545)"
fi
