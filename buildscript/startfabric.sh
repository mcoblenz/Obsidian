#!/bin/bash

# This script runs all the different Fabric commands needed to get the chaincode started,
# as well as running the chaincode jar and connecting to the proper address.
# It assumes the chaincode is stored in a file called 'chaincode.jar' under the folder
# '../build' (this file is produced by 'gradle build'.)

# The script color-codes the output of each process and prepends a tag so you can see
# the output of all Fabric processes without having to open several terminals.

# Note: Because this script is intended for internal development use, and not for
# use by people who are trying to actually operate blockchain things, it deletes the
# entire /var/hyperledger/production folder when re-run with the 'clean' option, since
# hyperledger doesn't really support removing channels or chaincodes. (for good reason!
# but when testing we want to be able to.) However, please be careful with this if you
# have other Hyperledger things going on on your machine!

# These set certain pause lengths so you can actually see what it is doing.
ENTER_PAUSE=0.4
BETWEEN_PAUSE=0.25

if [ -z "$1" ] ; then
    echo "Usage: $0 <path to chaincode.jar> [ clean ] [ <instantiation parameters> ]"
    echo "       for example: $0 build/chaincode.jar clean '\"a\",\"200\",\"b\",\"100\"'"
    echo "!!!! Warning: the 'clean' option will delete everything in /var/hyperledger/production."
    echo "!!!! Please be careful with it."
    echo "(However, without it, this script probably won't work multiple times.)"
    exit 1
fi

CCPATH=$1
CCDIR=$(dirname $CCPATH)

CCNAME="mycc"
CCVERSION=0

INIT_PARAMS=$2

if [ "$2" == "clean" ]; then
    rm -f ch1.block
    rm -rf /var/hyperledger/production
    mkdir /var/hyperledger/production
    INIT_PARAMS=$3
fi

pkill orderer || true
pkill peer || true

# Color settings
CNORM=$(tput setaf 15)
CORDR=$(tput setaf 12)
CPEER=$(tput setaf 6)
CCHAN=$(tput setaf 11)
CINST=$(tput setaf 9)
CCODE=$(tput setaf 10)

echo $CNORM'======= START ORDERER ======='
sleep $ENTER_PAUSE
ORDERER_GENERAL_GENESISPROFILE=SampleDevModeSolo orderer 2>&1 | sed "s/^/$CORDR[-orderer-] /" &
sleep 0.5
echo $CNORM'====== ORDERER STARTED ======'

sleep $BETWEEN_PAUSE

echo $CNORM'======== START PEER ========='
sleep $ENTER_PAUSE
peer node start --peer-chaincodedev 2>&1 | tee peer_output.tmp | sed "s/^/$CPEER[peer-node] /" &
sleep 1
echo $CNORM'======= PEER STARTED ========'

sleep $BETWEEN_PAUSE

echo $CNORM'====== CREATE CHANNEL ======='
sleep $ENTER_PAUSE
peer channel create -c ch1 -o localhost:7050 2>&1 | sed "s/^/$CCHAN[make-chan] /"
sleep 0.5
echo $CNORM'====== CREATED CHANNEL ======'

sleep $BETWEEN_PAUSE

echo $CNORM'======= JOIN CHANNEL ========'
sleep $ENTER_PAUSE
peer channel join -b ch1.block -o localhost:7050 2>&1 | sed "s/^/$CCHAN[join-chan] /"
sleep 0.5
echo $CNORM'====== JOINED CHANNEL ======='

sleep $BETWEEN_PAUSE

echo $CNORM'===== INSTALL CHAINCODE ====='
sleep $ENTER_PAUSE
peer chaincode install -l java -p $CCDIR -n $CCNAME -v $CCVERSION 2>&1 | sed "s/^/$CINST[-install-] /"
sleep 0.5
echo $CNORM'==== CHAINCODE INSTALLED ===='

sleep $BETWEEN_PAUSE

ip=$(cat peer_output.tmp | grep 'address=' | sed 's/.*address=//g' | sed 's/^\[//g' | sed 's/:.*$//' | head -1)
# echo -e "\e[96mIP address to connect to: " $ip
echo -e "\e[96m *** IP: $ip *** $CNORM"

sleep $BETWEEN_PAUSE

echo $CNORM'======= RUN CHAINCODE ======='
sleep $ENTER_PAUSE
java -jar $CCPATH -a $ip:7052 -i $CCNAME:$CCVERSION 2>&1 | sed "s/^/$CCODE[chaincode] /" &
sleep 5
echo $CNORM'===== CHAINCODE RUNNING ====='

sleep $BETWEEN_PAUSE

if [ -n "$INIT_PARAMS" ]; then
    echo $CNORM'=== INSTANTIATE CHAINCODE ==='
    echo -e "\e[96mInstantiation command: {\"Args\":[\"init\",$INIT_PARAMS]}"$CNORM
    sleep $ENTER_PAUSE
    peer chaincode instantiate -n $CCNAME -v $CCVERSION -l java -C ch1 -c "{\"Args\":[\"init\",$INIT_PARAMS]}" 2>&1 | sed "s/^/$CINST[-instant-] /"
    sleep 1
    echo -e "\e[96m(waiting for the blockchain!!)$CNORM"
    sleep 2.5
    echo $CNORM'=== CHAINCODE INSTANTIATED ==='
else
    echo -e "\e[96m  *** Note: No instantiation provided. ***$CNORM"
    echo -e "\e[96m  *** Please run 'peer chaincode instantiate' manually. ***$CNORM"
fi

sleep $BETWEEN_PAUSE

echo -e "\e[96m  *** Great! We made it. *** $CNORM"

rm peer_output.tmp

sleep $BETWEEN_PAUSE

echo $CNORM'============ READY ==========='

sleep 1000000000
