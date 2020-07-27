#!/usr/bin/python3

import sys
import os
from os import path
import re
import json
import urllib.request

CACHE = "etherscan_cache/"

BASEURL = "https://api.etherscan.io/api?module=contract&action=getsourcecode&address=%s&apikey=%s"


def is_cached(address) :
    return os.path.isfile(CACHE + address + ".sol")

def save_file(address, source_code) :
    if not path.isdir(CACHE) :
        os.mkdir(CACHE)
    file = open(CACHE + address + ".sol", "x")
    file.write(source_code)
    file.close()
    return 0

def buildURL(address, APIkey) :
    return BASEURL % (address, APIkey)

def rip_etherscan(etherscan_address, APIkey) :
    if not re.search(r'^0x[0-9A-Fa-f]{40}$', etherscan_address) :
        print("Invalid etherscan address: " + etherscan_address, file=sys.stderr)
        return 1
    if not re.search(r'[0-9A-Z]{34}', APIkey) :
        print("Invalid API key: " + APIkey, file=sys.stderr)
        return 1
    if is_cached(etherscan_address) :
        return 0

    #The code in this section was taken from etherscan.py in crytic-compile/platforms with minor modification
    response = urllib.request.urlopen(buildURL(etherscan_address, APIkey))
    html = response.read()
    info = json.loads(html)
    if "message" not in info:
        print("Incorrect etherscan request", file=sys.stderr)
        return 1
    if not info["message"].startswith("OK"):
        print("Contract has no public source code", file=sys.stderr)
        return 1
    if "result" not in info or "SourceCode" not in info["result"][0] :
        print("Contract has no public source code 2", file=sys.stderr)
        return 1
 
    return save_file(etherscan_address, info["result"][0]["SourceCode"])

if __name__ == "__main__" :
    #The first argument should be the etherscan address, the second should be the etherscan API key
    rip_etherscan(sys.argv[1], sys.argv[2])
