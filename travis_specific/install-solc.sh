#!/bin/sh

set -e
# check to see if solc folder is empty
if [ ! -d "${HOME}/solc/" ]; then
  sudo add-apt-repository ppa:ethereum/ethereum
  sudo apt-get update
  sudo apt-get install solc

else
    echo "Using cached directory."
fi

