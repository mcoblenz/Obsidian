#!/usr/bin/python

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


# return { result -> pass/fail, progress -> list of strings, reason -> string, non empty if we're in fail }
def run_one_test(test_info, verbose):
    ganache_host = "http://localhost:8545"
    obsidian_jar = "" # todo
    progress = []
    # compile the obsidian file in question to yul with a jar of obsidianc
    comp_result = subprocess.run(
        ["java", "-jar", obsidian_jar, "--yul", f"resources/tests/GanacheTests/{test_info['file']}.obs"], capture_output=True)
    if not comp_result.returncode == 0:
        return {'result': "fail", 'progress': progress, "reason": f"obsidianc run failed with output {comp_result.stderr}"}
    else:
        progress = progress + ["compiled obsidian to yul"]

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

    return {'result': "pass", 'progress': progress, "reason": ""}



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
files_with_tests = [test['file'] for test in tests_data['tests']]
files_present = [os.path.basename(obs) for obs in glob.glob(test_dir + '*.obs')]

extra_files = list(set(files_present) - set(files_with_tests))
extra_test_descriptions = list(set(files_with_tests) - set(files_present))

if extra_files:
    warn("there are obsidian files present that are not described by the test JSON file:\n\t" + pprint.pformat(
        extra_files))

if extra_test_descriptions:
    warn("there are described tests that do not have present obsidian files:\n\t" + pprint.pformat(
        extra_test_descriptions))

verbose = True

# todo: make the obsidianc jar
if verbose:
    print("running sbt build")

build = subprocess.run(["sbt" "\'set assembly / test := {}\'", ++"$TRAVIS_SCALA_VERSION", "assembly"], capture_output=True)

# todo: add ability to do one offs not just the whole suite
failed = []
for test in tests_data['tests']:
    result = run_one_test(test, False)
    if result['result'] == "pass":
        print(colored("PASS:", 'green'), test['file'])
    elif result['result'] == "fail":
        print(colored("FAIL:", 'red'), test['file'])
        print(f"\t{result}")
        failed = failed + [test['file']]
    else:
        error(f"test script error: result from test was neither pass nor fail, got {result['result']}")

if len(failed) == 0:
    print(colored(f"\nALL {str(len(tests_data['tests']))} TESTS PASSED", 'green'))
    sys.exit(0)
else:
    print(colored(f"\n{len(failed)}/{str(len(tests_data['tests']))} TESTS FAILED", 'red'))
    sys.exit(1)
