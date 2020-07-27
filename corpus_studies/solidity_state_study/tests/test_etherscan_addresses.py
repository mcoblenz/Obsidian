#!/usr/bin/python3

import sys
import os
import re
import subprocess as sub
import rip_etherscan
import set_solc_version


def gen_contract_address(address) :
    return "etherscan_cache/" + address + ".sol"

if __name__ == "__main__" :
    addresses_file_path = sys.argv[1]
    addresses_file = open(addresses_file_path, "r")
    while True :

        address = addresses_file.readline()
        address = address.replace("\n", "")

        if not address :
            break
        if not re.search(r'^0x[0-9A-Fa-f]{40}$', address) :
            print("Invalid etherscan address: " +  address)
        else :
            rip_etherscan.rip_etherscan(address, sys.argv[2])
            success = set_solc_version.set_version(gen_contract_address(address))
            if success :
                sub.run(["slither", gen_contract_address(address), "--solc-disable-warnings", "--detect", "reentrancy-eth,reentrancy-unlimited-gas,reentrancy-no-eth"])
    addresses_file.close()
