#!/usr/local/bin/python3


# This script rips code from etherscan and saves it into a cache so that it can be read by a compiler.

# It takes two arguments on the command line:
#    1. The etherscan address to rip
#    2. An etherscan API key

# The caching directory is denoted by the variable CACHE, so if you want to change the location on your
# own machine, change that variable. If you want to avoid caching altogether, have is_cached always return True.

import sys
import os
from os import path
import re
import json
import urllib.request

CACHE = "etherscan_cache/"
BASEURL = "https://api.etherscan.io/api?module=contract&action=getsourcecode&address=%s&apikey=%s"

def cache() :
    return CACHE

def is_cached(address) :
    return os.path.isfile(CACHE + address + ".sol")

def save_file(address, source_code) :
    if not path.isdir(CACHE) :
        os.mkdir(CACHE)
    file = open(CACHE + address + ".sol", "w")
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
    rip_etherscan(sys.argv[1], sys.argv[2])
