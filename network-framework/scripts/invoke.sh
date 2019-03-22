#!/bin/bash


CHANNEL_NAME="mychannel"
DELAY="3"
LANGUAGE="java"
TIMEOUT="10"
VERBOSE="false"
ORDERER_CA=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/ordererOrganizations/example.com/orderers/orderer.example.com/msp/tlscacerts/tlsca.example.com-cert.pem
HARD_PEER_CONN_PARMS="--peerAddresses peer0.org1.example.com:7051
 --tlsRootCertFiles /opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org1.example.com/peers/peer0.org1.example.com/tls/ca.crt
 --peerAddresses peer0.org2.example.com:7051
 --tlsRootCertFiles /opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org2.example.com/peers/peer0.org2.example.com/tls/ca.crt "

PROTO_PATH="${CC_SRC_PATH}protos/"
CC_NAME=$(basename $CC_SRC_PATH)


verifyResult() {
  if [ $1 -ne 0 ]; then
    echo "!!!!!!!!!!!!!!! "$2" !!!!!!!!!!!!!!!!"
    echo "========= ERROR !!! FAILED to execute End-2-End Scenario ==========="
    echo
    exit 1
  fi
}


parseParameters() {
  # If there is no argument left, no function name is specified
  # exit
  if [ "$#" = 0 ]; then
    exit 1
  fi

  # set -x
  PARAMS='{"Args":['

  # Parse out all arguments
  while [ "$#" -gt 1 ]; do
    if [ "$1" = "-g" ]; then
        if [ "$#" -lt 4 ]; then break; fi
        # Interpret the NEXT arguments as a TYPE followed by a GUID.
        shift
        TYPE=$1
        shift
        GUID=$1
        ENCODED_GUID=$(echo -n "guid: \"${GUID}\"" | protoc --encode=$TYPE --proto_path=$PROTO_PATH ${PROTO_PATH}${CC_NAME}OuterClass.proto)
        BASE64_GUID=$(echo -n "$ENCODED_GUID" | openssl base64)
        PARAMS="$PARAMS\"$BASE64_GUID\","
    else
        PARAMS="$PARAMS\"$1\","
    fi
    # shift by one to get the next function name or argument for function call
    shift
  done

  # Ugh, bash is terrible. Refactor this!
  if [ "$1" = "-g" ]; then
        # Interpret the NEXT arguments as a TYPE followed by a GUID.
        shift
        TYPE=$1
        shift
        GUID=$1
        ENCODED_GUID=$(echo -n "guid: \"${GUID}\"" | protoc --encode=$TYPE --proto_path=$PROTO_PATH ${PROTO_PATH}${CC_NAME}OuterClass.proto)
        BASE64_GUID=$(echo -n "$ENCODED_GUID" | openssl base64)
        PARAMS="$PARAMS\"$BASE64_GUID\""
    else
        PARAMS="$PARAMS\"$1\""
    fi



  PARAMS="$PARAMS]}"
}

# Check if invocation is run in quiet mode
isQuiet=0
while getopts "q" opt; do
  case "$opt" in
  q)
    isQuiet=1
    shift
    ;;
   esac
done

parseParameters $@
res=$?
verifyResult $res "Invoke transaction failed on channel '$CHANNEL_NAME' due to uneven number of peer and org parameters
or lack of function name"
if [ $isQuiet -eq 1 ]; then
  peer chaincode invoke -o orderer.example.com:7050 --tls $CORE_PEER_TLS_ENABLED --cafile $ORDERER_CA -C $CHANNEL_NAME -n mycc $HARD_PEER_CONN_PARMS -c $PARAMS >&log.txt
  res=$?
  verifyResult $res "Invoke execution on $PEERS failed "
  cat log.txt | sed -nr 's/^.*?payload:\"(.*)\".*$/\1/p'
else
  set -x
  peer chaincode invoke -o orderer.example.com:7050 --tls $CORE_PEER_TLS_ENABLED --cafile $ORDERER_CA -C $CHANNEL_NAME -n mycc $HARD_PEER_CONN_PARMS -c $PARAMS >&log.txt
  res=$?
  set +x
  cat log.txt
  verifyResult $res "Invoke execution on $PEERS failed "
  echo "===================== Invoke transaction successful on $PEERS on channel '$CHANNEL_NAME' ===================== "
  echo
fi