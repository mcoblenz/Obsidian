#!/usr/bin/python

import glob
import json
import os
import pprint
import subprocess
import sys
import argparse
from shutil import which

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
def run_one_test(test_info, verbose, obsidian_jar):
    ganache_host = "http://localhost:8545"
    progress = []
    # compile the obsidian file in question to yul with a jar of obsidianc
    comp_result = subprocess.run(
        ["java", "-jar", obsidian_jar, "--yul", f"resources/tests/GanacheTests/{test_info['file']}"],
        capture_output=True)
    if not comp_result.returncode == 0:
        return {'result': "fail", 'progress': progress,
                "reason": f"obsidianc run failed with output {comp_result.stderr}"}
    else:
        progress = progress + ["compiled obsidian to yul"]

    # compile the yul to evm with solc
    # docker run -v "$( pwd -P )":/sources ethereum/solc:stable --bin --strict-assembly --optimize /sources/"$NAME".yul > "$NAME".evm

    # start up a ganache process (todo: can i run all the tests in one to save time?)
    # ganache-cli --host localhost --gasLimit "$GAS" --accounts="$NUM_ACCT" --defaultBalanceEther="$START_ETH" &> /dev/null &

    # poll for an account to let it start up

    # send a transaction

    # get a transaction receipt to get the contract address

    # get the contract address from the transaction receipt

    # use call and the contract address to get the result of the function

    # pull the result out of the JSON objec

    # decode the logs from the bloom filter, if the test JSON includes a requirement for logs

    # kill ganache

    return {'result': "pass", 'progress': progress, "reason": ""}


parser = argparse.ArgumentParser()
parser.add_argument("-v", "--verbose", help="increase output verbosity",
                    action="store_true")
parser.add_argument('tests', nargs='*',
                    help='names of tests to run; if this is empty, then we run all the tests', default=[])
args = parser.parse_args()

# todo; grab this off the commandline, also verbosity
test_dir = 'resources/tests/GanacheTests/'

# check to make sure the tools we need are installed and print versions; error otherwise
cmds = ["ganache-cli", "node", "npm"]  # todo add: java, solc
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

if args.verbose:
    print("running sbt build")

# todo: os.environ.get("TRAVIS_SCALA_VERSION") <-- maybe in the make file
build = subprocess.run(["make", "notest"], capture_output=True)  # todo output here
if not build.returncode == 0:
    print(build)
    error(build.stdout.decode("utf8"))

jar_path = glob.glob("target/scala*/obsidianc.jar")
if not jar_path:
    error("could not find an obsidianc jar file after running sbt")

if args.verbose:
    print(f"using top of {pprint.pformat(jar_path)}")

# todo gross
tests_to_run = tests_data['tests']
if args.tests:
    tests_to_run = list(filter(lambda t: os.path.splitext(t['file'])[0] in set(args.tests), tests_data['tests']))

if args.verbose:
    if args.tests:
        print(f"running only these tests:\n{pprint.pformat(tests_to_run)}")
    else:
        print(f"no tests specified, so running the whole suite:\n{pprint.pformat(tests_to_run)}")

# todo: bark if the tests specified aren't in the json file, too.

failed = []
for test in tests_to_run:
    result = run_one_test(test, args.verbose, jar_path[0])
    if result['result'] == "pass":
        print(colored("PASS:", 'green'), test['file'])
    elif result['result'] == "fail":
        print(colored("FAIL:", 'red'), test['file'])
        print(f"\t{result}")
        failed = failed + [test['file']]
    else:
        error(f"test script error: result from test was neither pass nor fail, got {result['result']}")

if failed:
    print(colored(f"\n{len(failed)}/{str(len(tests_to_run))} TESTS FAILED", 'red'))
    sys.exit(1)
else:
    print(colored(f"\nALL {str(len(tests_to_run))} TESTS PASSED", 'green'))
    sys.exit(0)
