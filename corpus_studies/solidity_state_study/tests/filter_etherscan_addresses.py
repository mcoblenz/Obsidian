#!/usr/local/bin/python3

# This script filters a file of etherscan addresses, leaving only addresses that can actually be 
# ripped and compiled

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
import time

def gen_contract_address(address) :
    return "etherscan_cache/" + address + ".sol"

def filter_etherscan_addresses(address_file_path, api_key) :
    addresses_file = open(address_file_path, "r")
    filtered_file = open(address_file_path + ".tmp", "w+")
    while True :
        address = addresses_file.readline()
        address = address.replace("\n", "")

        if not address :
            break
        if not re.search(r'^0x[0-9A-Fa-f]{40}$', address) :
            print("Invalid etherscan address: " +  address)
        else :
            rip_etherscan.rip_etherscan(address, api_key)
            success = set_solc_version.set_version(gen_contract_address(address))
            if success :
                filtered_file.write(address + "\n")
            else :
                print("No valid compiler version found")
            time.sleep(0.5)
    addresses_file.close()
    filtered_file.close()
    os.remove(addresses_file_path)
    os.rename(addresses_file_path + ".tmp", addresses_file_path)


if __name__ == "__main__" :
    filter_etherscan_addresses(sys.argv[1], sys.argv[2])