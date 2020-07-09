#!/bin/zsh 

echo $1
echo $2

while read p; do
  echo "$p"
  ./rip_etherscan.py $p $2
  ./set_solc_version.py "etherscan_cache/${p}.sol"
  slither "etherscan_cache/${p}.sol" --solc-disable-warnings --detect hasstate,statenum
done <$1
