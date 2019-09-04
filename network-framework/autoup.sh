#!/bin/bash
# This script invokes either up.sh or upgrade.sh, depending on whether the network is running.

function checkNetworkIsUp() {
    CLI=$(docker ps -f name=cli -q)
    if [ -n "$CLI" ]; then
      NETWORKISUP=1
    else
      echo "no network"
      NETWORKISUP=0
    fi

}

function printHelp() {
  echo "autoup.sh conditionally starts a new Fabric network or upgrades an existing one."
  echo "Usage: "
  echo "  autoup.sh -s <chaincodedirectory>"
  echo "     -s <chaincodedirectory> - the path to the chaincode directory"
}

CD=default

while getopts "h?c:t:d:f:s:l:i:v:n:" opt; do
  case "$opt" in
  h | \?)
    printHelp
    exit 0
    ;;
  c)
    CHANNEL_NAME=$OPTARG
    ;;
  t)
    CLI_TIMEOUT=$OPTARG
    ;;
  d)
    CLI_DELAY=$OPTARG./
    ;;
  f)
    COMPOSE_FILE=$OPTARG
    ;;
  s)
    CD=${OPTARG}
    ;;
  l)
    LANGUAGE=$OPTARG
    ;;
  i)
    IMAGETAG=$(go env GOARCH)"-"$OPTARG
    ;;
  v)
    VERBOSE=true
    ;;
  n)
    if [ -n "$INIT" ]; then
        INIT="${INIT},\"${OPTARG}\""
    else
        INIT="\"${OPTARG}\""
    fi
    ;;
  esac
done

checkNetworkIsUp
NETWORKDIR=$(dirname $BASH_SOURCE)

# Find out if this is the same chaincode path as before.
FOUND=$(grep -c -e "^${CD}$" .env)
if [ $? -eq 0 ]; then #same chaincode path as before
  if [ $NETWORKISUP -ne 0 ]; then
    echo "network is up; upgrading"
     ${NETWORKDIR}/upgrade.sh
  else
    echo "network down; starting"
    ${NETWORKDIR}/up.sh $@
  fi
else
  echo "different chaincode; restarting"
  if [ $NETWORKISUP -ne 0 ]; then
    ${NETWORKDIR}/down.sh
  fi
  ${NETWORKDIR}/up.sh $@
fi
