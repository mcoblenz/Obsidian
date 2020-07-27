#!/usr/bin/python3
#In order to run this script you must use python 3.5 or later

import sys
import subprocess as sub
import os
import re
import semantic_version as sv


def is_compatable_version(version_range, version) :
    range = sv.NpmSpec(version_range)
    try : 
        version_obj = sv.Version(version)
    except ValueError :
        return False
    return range.match(version_obj)

#This script relies on solc being the version from trailofbits/solc-select. If you're using a version with a different mechanism for choosing
#the compiler version, you may need to change this function.
def get_solc_versions() :
    process = sub.run(["solc", "--versions"], stdout=sub.PIPE, encoding='ascii')
    return process.stdout.split("\n")

def get_solc_pragma(file) :
    while True :
        line = file.readline()

        if not line :
            break
        if "pragma" in line and "//" not in line:
            version = line.replace(";","")
            return version

    return ""

def set_version(file_path) :
    file = open(file_path, "r")
    version_line = get_solc_pragma(file)
    file.close()

    if not version_line :
        #No version of solc specified in input file.
        return False

    version_groups = version_line.replace("pragma solidity ", "")
    versions_available = get_solc_versions()

    compatable_versions = list(filter(lambda vers : vers != "nightly" and is_compatable_version(version_groups, vers), versions_available))
    if compatable_versions :
        #Set the solc version to the first available version
        sub.run(["solc", "use", compatable_versions[0]], stdout=sub.PIPE, encoding='ascii')
    else :
        #No compatable version of solc found
        return False

    return True

if __name__ == "__main__" :
    set_version(sys.argv[1])
