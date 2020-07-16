#!/usr/local/bin/python3

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
    filtered_file = open(addresses_file_path + ".tmp", "w+")
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
                try :
                    sub.run(["slither", gen_contract_address(address), "--solc-disable-warnings", "--detect", "hasstate"],stdout=sub.PIPE,encoding='ascii')
                    filtered_file.write(address + "\n")
                except :
                    pass
    addresses_file.close()
    filtered_file.close()
    os.remove(addresses_file_path)
    os.rename(addresses_file_path + ".tmp", addresses_file_path)
