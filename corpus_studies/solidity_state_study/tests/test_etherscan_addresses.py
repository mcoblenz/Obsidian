#!/usr/local/bin/python3

# This script runs slither on a list of etherscan addresses contained in a file.

# It takes two arguments on the command line: 
#    1. The name of the address list file
#         This file should contain etherscan addresses on each line, one per line. 
#         This file should contain no blank lines, as these signal the end of the file.
#    2. An etherscan API key


import sys
import os
import re
import subprocess as sub
import rip_etherscan
import set_solc_version

# To run slither with different detectors, change this variable to contain the 
# names of the slither detectors you want to run. If you are using custom detectors,
# make sure you are in a slither-dev python environment, otherwise they won't show up.
DETECTORS = "reentrancy-eth,reentrancy-unlimited-gas,reentrancy-no-eth"

def gen_contract_address(cache_name, address) :
    return cache_name + address + ".sol"

def test_etherscan_addresses(address_file_path, api_key) :
    addresses_file = open(addresses_file_path, "r")
    while True :
        address = addresses_file.readline()
        address = address.replace("\n", "")

        if not address :
            break
        if not re.search(r'^0x[0-9A-Fa-f]{40}$', address) :
            print("Invalid etherscan address: " +  address)
        else :
            rip_etherscan.rip_etherscan(address, api_key)
            success = set_solc_version.set_version(gen_contract_address(rip_etherscan.cache(), address))
            if success :
                sub.run(["slither", gen_contract_address(address), "--solc-disable-warnings", "--detect", DETECTORS])
    addresses_file.close()


if __name__ == "__main__" :
    test_etherscan_addresses(sys.argv[1], sys.argv[2])