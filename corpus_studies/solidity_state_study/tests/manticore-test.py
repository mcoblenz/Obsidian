#!/usr/local/bin/python3

from manticore.ethereum import *

if __name__ == "__main__" :
    m = ManticoreEVM()
    account = m.create_account(address=m.new_address())

    address = m.create_account(address=m.new_address())

    source_code = ""
    file = open("etherscan_cache/0x05f4a42e251f2d52b8ed15e9fedaacfcef1fad27.sol", "r")
    source_code += file.read()
    file.close

    contract_name = "Pausable"
    account = m.solidity_create_contract(source_code, account, contract_name=contract_name)


    print(m.count_all_states())

    for s in m.all_states :
        print (str(s))