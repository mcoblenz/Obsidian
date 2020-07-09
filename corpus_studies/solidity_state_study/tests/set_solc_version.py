#!/usr/local/bin/python3
#In order to run this script you must use python 3.5 or later

import sys
import subprocess as sub
import os
import re

def is_compatable_version(version_needed, version) :
    version_numbers_needed = version_needed.split(".")
    version_numbers = version.split(".")
    if len(version_numbers_needed) != len(version_numbers) or len(version_numbers_needed) != 3 :
        return False
    
    return version_numbers_needed[1] == version_numbers[1] and int(version_numbers_needed[2]) <= int(version_numbers[2])

#This script relies on solc being the version from trailofbits/solc-select. If you're using a version with a different mechanism for choosing
#the compiler version, you may need to change this function.
def get_solc_versions() :
    process = sub.run(["solc", "--versions"], stdout=sub.PIPE, encoding='ascii')
    return process.stdout.split("\n")

def get_solc_version(file) :

    while True :
        line = file.readline()

        if not line :
            break

        if "pragma" in line :
            
            version = line.split("^")[1].replace(";","")
            if len(version.split(".")) != 3:
                print("Unsupported compiler specification, aborting.", file=sys.stderr)
            return version

    return ""

def set_version(file_path) :
    file = open(file_path, "r")
    version_needed = get_solc_version(file)
    file.close()

    if not version_needed :
        print("No version of solc specified in input file, aborting.", file=sys.stderr)

    versions_available = get_solc_versions()

    compatable_versions = list(filter(lambda vers : is_compatable_version(version_needed, vers), versions_available))
    if compatable_versions :
        #Set the solc version to the first available version
        sub.run(["solc", "use", compatable_versions[0]])
    else :
        print("No compatable version of solc found, aborting.", file=sys.stderr)
        return 1

    return 0

if __name__ == "__main__" :
    set_version(sys.argv[1])
