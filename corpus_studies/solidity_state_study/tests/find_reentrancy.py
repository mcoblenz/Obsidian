#!/usr/local/bin/python3

import sys
import os
import re
import subprocess as sub
import rip_etherscan
import set_solc_version
from functools import reduce
import time

TAB = "    "

def truncate(f, n):
    # Taken from stack overflow post https://stackoverflow.com/questions/783897/truncating-floats-in-python, user David Z
    s = '{}'.format(f)
    i, p, d = s.partition('.')
    return '.'.join([i, (d+'0'*n)[:n]])

def write_results(file, address_count, function_storage) :
    flagged_address_count = len(function_storage.keys())
    flagged_contract_count = str(reduce(lambda x,y: x+y, [len(function_storage[elem]) for elem in function_storage.keys()]))
    flagged_address_percentage = str((flagged_address_count/address_count) * 100)
    flagged_address_count = str(flagged_address_count)
    file.write("In total, " + str(address_count) + " addresses were read.\n")
    file.write("Of those, " + flagged_address_count + " addresses were flagged for reentrancy (" + truncate(flagged_address_percentage,2) + " percent), ")
    file.write("and " + flagged_contract_count + " contracts were flagged for reentrancy.\n\n\n")

    file.write("The following functions were flagged for reentrancy:\n")

    for address in function_storage.keys() :
        file.write("In address " + address + ":\n")
        for contract in function_storage[address].keys() :
            file.write(TAB + "In contract " + contract + ":\n")
            for func in function_storage[address][contract] :
                file.write(TAB + TAB + func + "\n")
        file.write("\n")

def gen_contract_address(address) :
    return "etherscan_cache/" + address + ".sol"

if __name__ == "__main__" :
    addresses_file_path = sys.argv[1]
    addresses_file = open(addresses_file_path, "r")

    address_count = 0

    # This is a nested dictionary of type Dict[Str,Dict[Str,List[Str]]]
    # The outer keys are addresses, the inner keys are contract names. 
    # The innermost values are function names.
    reentrant_function_storage = {}
    
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
                process = sub.run(["slither", gen_contract_address(address), "--solc-disable-warnings", "--detect", "reentrancy-eth,reentrancy-no-eth"], stdout=sub.PIPE,stderr=sub.PIPE, encoding='utf-8')


                errput = process.stderr.split("\n")
                address_count += 1
            
                for line in errput :
                    if "Reentrancy in " in line :
                        full_func = line.replace("Reentrancy in ", "").split(" ")[0]
                        func_split = full_func.split(".")
                        contract = func_split[0]
                        function = func_split[1]

                        address_storage = reentrant_function_storage.get(address)
                        contract_dict = {} if address_storage is None else address_storage

                        contract_dict[contract] = ([] if contract_dict.get(contract) is None else contract_dict[contract]) + [function]
                        
                        reentrant_function_storage[address] = contract_dict



                
    addresses_file.close()
    results_file = open("reentrancy_results.txt", "w+")
    results_file.seek(0)
    write_results(results_file, address_count, reentrant_function_storage)
    results_file.truncate()
    results_file.close()
