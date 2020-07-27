#!/usr/bin/python3

from manticore.ethereum import *

if __name__ == "__main__" :
    m = ManticoreEVM()
    account = m.create_account(address=m.new_address())

    address = m.create_account(address=m.new_address())

    source_code = ""
    file = open("etherscan_cache/0xa15c7ebe1f07caf6bff097d8a589fb8ac49ae5b3.sol", "r")
    source_code += file.read()
    file.close

    contract_name = "Pausable"
    account = m.solidity_create_contract(source_code, account, contract_name=contract_name)


    print(m.count_all_states())

    for s in m.all_states :
        print (str(s))
