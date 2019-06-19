#!/bin/bash

: ${CHANNEL_NAME:="mychannel"}
: ${DELAY:="3"}
: ${LANGUAGE:="java"}
: ${TIMEOUT:="10"}
: ${VERBOSE:="false"}
LANGUAGE="$(echo "$LANGUAGE" | tr [:upper:] [:lower:])"

PEER="$1"
ORG="$2"
INIT_ARGS="$3"
VERSION=${4:-1.0}

ORDERER_CA=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/ordererOrganizations/example.com/orderers/orderer.example.com/msp/tlscacerts/tlsca.example.com-cert.pem
PEER0_ORG1_CA=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org1.example.com/peers/peer0.org1.example.com/tls/ca.crt
PEER0_ORG2_CA=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org2.example.com/peers/peer0.org2.example.com/tls/ca.crt
PEER0_ORG3_CA=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org3.example.com/peers/peer0.org3.example.com/tls/ca.crt

SCRIPTS_PATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/scripts

if [ -n "${INIT_ARGS}" ] ; then
    if [ "${INIT_ARGS}" == "-" ] ; then
        INIT='["init"]'
    else
        INIT='["init",'${INIT_ARGS}']'
    fi
else
    INIT='["init"]'
fi

verifyResult() {
  if [ $1 -ne 0 ]; then
    echo "!!!!!!!!!!!!!!! "$2" !!!!!!!!!!!!!!!!"
    echo "========= ERROR !!! FAILED to execute End-2-End Scenario ==========="
    echo
    exit 1
  fi
}

setGlobals() {
  PEER=$1
  ORG=$2
  if [ $ORG -eq 1 ]; then
    CORE_PEER_LOCALMSPID="Org1MSP"
    CORE_PEER_TLS_ROOTCERT_FILE=$PEER0_ORG1_CA
    CORE_PEER_MSPCONFIGPATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org1.example.com/users/Admin@org1.example.com/msp
    if [ $PEER -eq 0 ]; then
      CORE_PEER_ADDRESS=peer0.org1.example.com:7051
    else
      CORE_PEER_ADDRESS=peer1.org1.example.com:7051
    fi
  elif [ $ORG -eq 2 ]; then
    CORE_PEER_LOCALMSPID="Org2MSP"
    CORE_PEER_TLS_ROOTCERT_FILE=$PEER0_ORG2_CA
    CORE_PEER_MSPCONFIGPATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org2.example.com/users/Admin@org2.example.com/msp
    if [ $PEER -eq 0 ]; then
      CORE_PEER_ADDRESS=peer0.org2.example.com:7051
    else
      CORE_PEER_ADDRESS=peer1.org2.example.com:7051
    fi

  elif [ $ORG -eq 3 ]; then
    CORE_PEER_LOCALMSPID="Org3MSP"
    CORE_PEER_TLS_ROOTCERT_FILE=$PEER0_ORG3_CA
    CORE_PEER_MSPCONFIGPATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org3.example.com/users/Admin@org3.example.com/msp
    if [ $PEER -eq 0 ]; then
      CORE_PEER_ADDRESS=peer0.org3.example.com:7051
    else
      CORE_PEER_ADDRESS=peer1.org3.example.com:7051
    fi
  else
    echo "================== ERROR !!! ORG Unknown =================="
  fi

  if [ "$VERBOSE" == "true" ]; then
    env | grep CORE
  fi
}

# Install chaincode on all peers
for org in 1 2; do
    bash "$SCRIPTS_PATH/install.sh" 0 "$org" "$VERSION"
done

echo "===================== Upgrading chaincode on peer${PEER}.org${ORG} ===================== "
setGlobals $PEER $ORG
set -x
peer chaincode upgrade -o orderer.example.com:7050 -C "$CHANNEL_NAME" -n mycc\
    --tls "$CORE_PEER_TLS_ENABLED" --cafile "$ORDERER_CA" -l "${LANGUAGE}"\
    -v "${VERSION}" -c "{\"Args\":${INIT}}" -p "${CC_SRC_PATH}"\
    -P "AND ('Org1MSP.peer','Org2MSP.peer')" >&log.txt
res=$?
set +x
cat log.txt
verifyResult $res "Chaincode upgrade on peer${PEER}.org${ORG} has failed"
echo "===================== Chaincode is upgraded on peer${PEER}.org${ORG} ===================== "
echo

