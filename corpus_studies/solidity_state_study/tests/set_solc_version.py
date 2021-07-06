#!/usr/local/bin/python3
# In order to run this script you must use python 3.5 or later

# This script relies on the version of solc from trailofbits/solc-select. If you're using a version with a different mechanism for choosing
# the compiler version, you may need to change this script.

# The script expects one argument on the command line:
#    1. The file path of the solidity file to change the compiler version for

import sys
import subprocess as sub
import os
import re
import semantic_version as sv

def is_compatible_version(version_range, version):
    try:
        range = sv.NpmSpec(version_range)
        version_obj = sv.Version(version)
        return range.match(version_obj)
    except ValueError:
        return False

def get_solc_versions() :
    process = sub.run(["solc-select", "versions"], stdout=sub.PIPE, encoding='ascii')
    versions = process.stdout.split("\n")
    versions = filter(len, versions)
    versions = list(map(lambda v: v.split(' ',1)[0],versions))
    return versions

def get_solc_pragma(file) :
    while True :
        line = file.readline()

        if not line :
            break
        if line.startswith("pragma solidity"):
            line.replace('"','')
            version = line.split(';')[0]
            return version

    return ""

def set_version_from_string(version):
    sub.run(["solc-select", "use", version], stdout=sub.PIPE, encoding='ascii')


def set_version(file_path) :
    file = open(file_path, "r")
    version_line = get_solc_pragma(file)
    file.close()

    if not version_line :
        #No version of solc specified in input file.
        return False

    version_groups = version_line.replace("pragma solidity ", "")
    versions_available = get_solc_versions()

    compatible_versions = list(filter(lambda vers : vers != "nightly" and is_compatible_version(version_groups, vers), versions_available))
    if compatible_versions :
        #Set the solc version to the first available version
        set_version_from_string(compatible_versions[0])
    else :
        #No compatible version of solc found
        return False

    return True

if __name__ == "__main__" :
    set_version(sys.argv[1])
