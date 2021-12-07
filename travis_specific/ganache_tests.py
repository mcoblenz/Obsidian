import glob
import json
import os
import pprint
import sys
import subprocess
from shutil import which
import eth_abi

from termcolor import colored


def warn(s):
    print(colored("WARNING:", 'yellow'), str(s))


def error(s):
    print(colored("ERROR:", 'red'), str(s))
    sys.exit(1)


def check_for_command(cmd):
    if not which(cmd):
        error(f"{cmd} not installed")


def run_one_test(test_info, verbose):
    ganache_host = "http://localhost:8545"
    # compile the obsidian file in question to yul with a jar of obsidianc

    # compile the yul to evm with solc

    # start up a ganache process (todo: can i run all the tests in one to save time?)

    # poll for an account to let it start up

    # send a transaction

    # get a transaction receipt to get the contract address

    # get the contract address from the transaction receipt

    # use call and the contract address to get the result of the function

    # pull the result out of the JSON objec

    # decode the logs from the bloom filter, if the test JSON includes a requirement for logs

    # kill ganache 

    if len(test_info['file']) > 6:
        return None
    else:
        return "mock fail"


# todo; grab this off the commandline, also verbosity
test_dir = 'resources/tests/GanacheTests/'

# check to make sure the tools we need are installed and print versions; error otherwise
cmds = ["ganache-cli", "node", "npm"]
for c in cmds:
    check_for_command(c)
    version = subprocess.run([c, "--version"], capture_output=True)
    print(f"{c}\t{version.stdout.strip().decode()}")

# read the tests json file into a dictionary
f = open(test_dir + 'tests.json')
if not f:
    error("could not open tests.json file")
tests_data = json.load(f)
f.close()

# compare the files present to the tests described, producing a warning in either direction
files_with_tests = sorted([test['file'] for test in tests_data['tests']])
files_present = sorted(os.path.basename(obs) for obs in glob.glob(test_dir + '*.obs'))

extra_files = list(set(files_present) - set(files_with_tests))
extra_test_descriptions = list(set(files_with_tests) - set(files_present))

if extra_files:
    warn("there are obsidian files present that are not described by the test JSON file:\n\t" + pprint.pformat(
        extra_files))

if extra_test_descriptions:
    warn("there are described tests that do not have present obsidian files:\n\t" + pprint.pformat(
        extra_test_descriptions))

# todo: make the obsidianc jar

# todo: add ability to do one offs not just the whole suite
failed = []
for test in tests_data['tests']:
    result = run_one_test(test, False)
    if not result:
        print(colored("PASS:", 'green'), test['file'])
    else:
        print(colored("FAIL:", 'red'), test['file'])
        print(f"\t{result}")
        failed = failed + [test['file']]
