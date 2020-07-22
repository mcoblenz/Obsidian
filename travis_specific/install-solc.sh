#!/bin/sh

set -e
# check to see if solc folder is empty
if [ ! -d "${HOME}/solc/" ]; then
    mkdir -p ${HOME}/solc/
    wget https://github.com/ethereum/solidity/releases/download/v0.6.12/solidity_0.6.12.tar.gz
    tar -zxf solidity_0.6.12.tar.gz
    mv bin/solc ${HOME}/solc/
else
    echo "Using cached directory."
fi

