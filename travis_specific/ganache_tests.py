#!/usr/bin/python
import argparse
import binascii
import glob
import json
import os
import pprint
import subprocess
import sys
import socket
from shutil import which

import eth_abi
import polling
from termcolor import colored
from web3 import Web3


def warn(s):
    print(colored("WARNING:", 'yellow'), str(s))


def error(s):
    print(colored("ERROR:", 'red'), str(s))
    sys.exit(1)


def check_for_command(cmd):
    if not which(cmd):
        error(f"{cmd} not installed")


def twos_comp(val, bits):
    """compute the 2's complement of int value val"""
    if (val & (1 << (bits - 1))) != 0:
        val = val - (1 << bits)
    return val


def is_port_in_use(host, port):
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        return s.connect_ex((host, port)) == 0


ganache_host = 'localhost'
ganache_port = 8545
ganache_url = f"http://{ganache_host}:{str(ganache_port)}"


def run_one_test(test_info, verbose, obsidian_jar, defaults):
    """run a test given its info, verbosity, and the location of the jar file. this returns a dictionary of the form
       { "result" -> "pass" or "fail",
         "progress" -> list of strings describing how far we got in the test,
         "reason" -> string, which describes the failure or is empty if it's a pass,
         }
    """
    test_name = os.path.splitext(test_info['file'])[0]
    progress = []

    stashing = []
    if 'stashed' in test_info.keys():
        stashing = ["--stash"]

    #### compile the obsidian file in question to yul with a jar of obsidianc
    run_obsidianc = subprocess.run(
        ["java", "-jar", obsidian_jar, "--yul"] + stashing + [f"resources/tests/GanacheTests/{test_info['file']}"],
        capture_output=True)
    if not run_obsidianc.returncode == 0:
        return {'result': "fail", 'progress': progress,
                "reason": f"obsidianc run failed with output {run_obsidianc.stdout} and error {run_obsidianc.stderr};{run_obsidianc.args}"}
    else:
        progress = progress + ["obsidianc compiled obsidian to yul"]

    #### compile the yul to evm with solc
    run_solc = subprocess.run(["docker",
                               "run",
                               "-v", f"{os.getcwd().format()}/{test_name}/:/src",
                               "ethereum/solc:stable",
                               "--bin",
                               "--strict-assembly",
                               "--optimize",
                               f"/src/{test_name}.yul"], capture_output=True)
    if not run_solc.returncode == 0:
        return {'result': "fail", 'progress': progress,
                "reason": f"solc run failed with output {run_solc.stderr}"}
    else:
        progress = progress + ["solc compiled yul to evm"]

    evm_bytecode = run_solc.stdout.decode("utf8").split("\n")[4]

    #### start up a ganache process
    stdout_redirect = subprocess.PIPE
    if verbose:
        stdout_redirect = None
    run_ganache = subprocess.Popen(["ganache",
                                    "--logging.verbose",
                                    "--logging.debug",
                                    "--server.host", ganache_host,
                                    "--server.port", str(ganache_port),
                                    "--miner.blockGasLimit", str(test_info.get('gas', defaults['gas'])),
                                    "--wallet.totalAccounts", str(test_info.get('numaccts', defaults['numaccts'])),
                                    "--wallet.defaultBalance", str(test_info.get('startingeth', defaults['startingeth']))
                                    ], stdout=stdout_redirect)
    progress = progress + [f"started ganache-cli process: {str(run_ganache)}"]

    # step through the sequence of interaction with ganache, catching all errors so that they stay contained to this
    # test and so that we can kill the ganache process cleanly
    try:
        # open a connection to ganache and wait it connects
        w3 = Web3(Web3.HTTPProvider(ganache_url))
        polling.poll(w3.isConnected, step=0.2, max_tries=100)
        progress = progress + ["connected to web3 provider"]

        # get the account number
        account_number = w3.eth.accounts[0]
        progress = progress + ["got account from web3"]

        #### send a transaction
        deploy_transaction_hash = w3.eth.send_transaction({"from": account_number,
                                                           "gas": int(test_info.get('gas', defaults['gas'])),
                                                           "data": f"0x{evm_bytecode}"})
        progress = progress + ["sent deploy transaction"]

        #### warn if there's no expected result because this is as far as we go
        if not test_info['expected']:
            warn(f"no expected result given for test {test_name} so exiting early")
            run_ganache.kill()
            return {'result': "pass", 'progress': progress,
                    "reason": "nothing failed; note that no result was checked, though"}

        #### get a transaction receipt to get the contract address
        deploy_transaction_receipt = w3.eth.wait_for_transaction_receipt(deploy_transaction_hash)
        progress = progress + ["got deploy transaction receipt"]

        #### use call and the contract address to get the result of running the function locally to
        #### the node for the result of the code

        method_name = test_info.get('trans', defaults['trans'])
        method_types = test_info.get('types', defaults['types'])
        method_args = test_info.get('args', defaults['args'])
        hash_to_call = Web3.keccak(text=method_name + "(" + ",".join(method_types) + ")")[:4].hex()
        encoded_args = binascii.hexlify(eth_abi.encode_abi(method_types, method_args)).decode()

        call_reply = w3.eth.call({
            "from": account_number,
            "to": deploy_transaction_receipt.contractAddress,
            "data": f"{hash_to_call}{encoded_args}"
        })
        progress = progress + [f"made call to eth_call"]

        #### compare the result to the expected answer
        got = twos_comp(int(call_reply.hex(), 16), 8 * 32)
        expected = int(test_info['expected'])
        if not got == expected:
            raise RuntimeError(f"expected {expected} but got {got}")
        progress = progress + ["got matched expected"]

        ## invoking transaction for effects
        invoke_transaction_hash = w3.eth.send_transaction({
            "from": account_number,
            "to": deploy_transaction_receipt.contractAddress,
            "data": f"{hash_to_call}{encoded_args}"
        })
        progress = progress + ["sent transaction for invocation"]

        invoke_transaction_receipt = w3.eth.wait_for_transaction_receipt(invoke_transaction_hash)
        progress = progress + [f"got receipt for invocation"]

        if 'stashed' in test_info.keys():
            # for now, we assume that this is always going to be a list of integers.
            got_stashed = []
            expected_stashed = test_info['stashed']
            for i in range(len(expected_stashed)):
                storage_contents = w3.eth.get_storage_at(deploy_transaction_receipt.contractAddress,str(hex(0xc0decafe + (32 * i))))
                got_stashed = got_stashed + [twos_comp(int(storage_contents.hex(), 16), 8 * 32)]
            if not got_stashed == expected_stashed:
                raise RuntimeError(f"expected {expected_stashed} but got {got_stashed}")
            progress = progress + [f"stash looks like {pprint.pformat(got_stashed)} which matches the expected stashing"]

        #### get the logs
        logs = invoke_transaction_receipt.logs
        if logs:
            warn(f"produced logs!")
            if verbose:
                pprint.pprint(logs)

    except BaseException as err:
        run_ganache.kill()
        return {'result': 'fail', 'progress': progress, 'reason': f"caught an exception:{str(err)}"}

    #### kill ganache and return a pass
    run_ganache.kill()
    return {'result': "pass", 'progress': progress, "reason": "nothing failed"}


parser = argparse.ArgumentParser()
parser.add_argument("-v", "--verbose", help="increase output verbosity",
                    action="store_true")
parser.add_argument("-q", "--quick", help="take some short cuts to run quickly in a local, non-travis "
                                          "setting. note that this should NOT be taken as the standard, "
                                          "it's just a debugging convenience",
                    action="store_true")
parser.add_argument("-c", "--clean", help="delete intermediate files after the run is complete",
                    action="store_true")
parser.add_argument("-d", "--dir", help="directory that contains the test json and obsidian files",
                    type=str, default='resources/tests/GanacheTests/')
parser.add_argument('tests', nargs='*',
                    help='names of tests to run; if this is empty, then we run all the tests', default=[])
args = parser.parse_args()

# sanity check that there isn't anything on the port we'll use ganache for; this can happen between consecutive runs
# if ganache didn't exit cleanly
if is_port_in_use(ganache_host, ganache_port):
    error(f"ganache-cli won't be able to start up, port {int(ganache_port)} isn't free on {ganache_host}")

# sanity check that there is a docker daemon running, so that we don't get through the sbt build below only to have to
# rerun the script after launching Docker
run_dstats = subprocess.run(["docker", "stats", "--no-stream"], capture_output=True)
if not run_dstats.returncode == 0:
    error(f"cannot connect to a docker daemon")

# read the tests json file into a dictionary
test_filename = "tests.json"
f = open(args.dir + test_filename)
if not f:
    error(f"could not open {test_filename} file")
tests_data = json.load(f)
f.close()

# compare the files present to the tests described, producing a warning in either direction
files_with_tests = [test['file'] for test in tests_data['tests']]
files_present = [os.path.basename(obs) for obs in glob.glob(args.dir + '*.obs')]

extra_files = list(set(files_present) - set(files_with_tests))
extra_test_descriptions = list(set(files_with_tests) - set(files_present))

if extra_files:
    warn("there are obsidian files present that are not described by the test JSON file:\n\t" + pprint.pformat(
        extra_files))

if extra_test_descriptions:
    warn("there are described tests that do not have present obsidian files:\n\t" + pprint.pformat(
        extra_test_descriptions))

# todo: there's a fair amount of repetition here with splitting the file name and that sort of thing

# by default, we'll run the whole suite, but if there are positional arguments we'll do those instead (or error if
# they don't exist in the json file)
tests_to_run = tests_data['tests']
if args.tests:
    for x in args.tests:
        if x not in set(map(lambda t: os.path.splitext(t['file'])[0], tests_data['tests'])):
            error(f"{x} is not a defined test")
    # if there are no undefined names, filter the data from the json according to the arguments given
    tests_to_run = list(filter(lambda t: os.path.splitext(t['file'])[0] in set(args.tests), tests_data['tests']))

# if the clean flag is set, before running tests make sure that no directories with names that would collide exist
if args.clean:
    for x in tests_to_run:
        if os.path.isdir(os.path.splitext(x['file'])[0]):
            error(f"running {str(x['file'])} would delete an existing directory")

# if we're being chatty, print out the test cases we're about to run
if args.verbose:
    if args.tests:
        print(f"running only these tests:\n{pprint.pformat(tests_to_run)}")
    else:
        print(f"no tests specified, so running the whole suite:\n{pprint.pformat(tests_to_run)}")

# check to make sure the tools we need are installed and print versions; error otherwise
if not args.quick:
    cmds = ["ganache-cli", "node", "npm", "java", "docker"]  # todo add: solc
    print("-------- versions --------")
    for c in cmds:
        check_for_command(c)
        version = subprocess.run([c, "--version"], capture_output=True)
        print(f"{c}\t{version.stdout.strip().decode()}")
    print("--------------------------\n")
else:
    warn("taking a shortcut and not outputting version info or checking for commands")

# build the obsidian jar, unless it exists and we're in fast-and-dirty mode
if args.quick and glob.glob("target/scala*/obsidianc.jar"):
    warn("taking a shortcut and using an existing obsidianc jar")
else:
    if args.verbose:
        print("running sbt build")

    build = subprocess.run(["make", "notest"], capture_output=True)
    if not build.returncode == 0:
        print(build)
        error(build.stdout.decode("utf8"))

jar_path = glob.glob("target/scala*/obsidianc.jar")
if not jar_path:
    error("could not find an obsidianc jar file after running sbt")

if args.verbose:
    print(f"using top of {pprint.pformat(jar_path)}")

# run each test, keeping track of which ones fail
failed = []
for test in tests_to_run:
    result = run_one_test(test, args.verbose, jar_path[0], tests_data['defaults'])
    if result['result'] == "pass":
        print(colored("PASS:", 'green'), test['file'])
    elif result['result'] == "fail":
        print(colored("FAIL:", 'red'), test['file'])
        pprint.pprint(result, indent=4)
        failed = failed + [test['file']]
    else:
        error(f"test script error: result from test was neither pass nor fail, got {result['result']}")

    if args.clean:
        name = os.path.splitext(test['file'])[0]
        warn(f"removing directory for {name}")
        os.remove(f"{name}/{name}.yul")
        os.rmdir(f"{name}")

# print out a quick summary at the bottom of the test run
if failed:
    print(colored(f"\n{len(failed)}/{str(len(tests_to_run))} TESTS FAILED", 'red'))
    if args.verbose:
        pprint.pprint(failed)
    sys.exit(1)
else:
    print(colored(f"\nALL {str(len(tests_to_run))} TESTS PASSED", 'green'))
    sys.exit(0)
