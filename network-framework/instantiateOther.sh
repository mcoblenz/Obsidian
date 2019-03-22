#!/usr/bin/env bash

function printHelp() {
  echo "Usage: "
  echo "  instantiateOther.sh contract [arg] ..."
  echo "    where arg is an argument to be passed to the other contract's constructor."
}

confirmNetworkUp() {
    CLI=$(docker ps -f name=cli -q)
    if [ -z "$CLI" ]; then
        echo "Fabric network not found. Did you remember to run up.sh?"
        exit 0
    fi
}


if [ $# -lt 1 ]
then
    printHelp
    exit 0
fi

quietMode=0

while getopts "h?:q" opt; do
  case "$opt" in
  h | \?)
    printHelp
    exit 0
    ;;
  q)
    quietMode=1
    shift # shift off the option
    ;;
   esac
done

confirmNetworkUp
if [ $quietMode -eq 1 ] ; then
    ./invoke.sh -q __instantiateOther $@
else
    ./invoke.sh __instantiateOther $@
fi