
#!/usr/bin/python
import argparse
import binascii
import glob
import json
import os
import pprint
import subprocess
import sys
from shutil import which

import eth_abi
import httpx
import polling
from Crypto.Hash import keccak
from termcolor import colored


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
    import socket
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        return s.connect_ex((host, port)) == 0


ganache_host = 'localhost'
ganache_port = 8545
ganache_url = f"http://{ganache_host}:{str(ganache_port)}"


def run_one_test(test_info, verbose, obsidian_jar, defaults):
    """run a test given its info, verbosity, and the location of the jar file. this returns a dictionary of the form
       { "result" -> "pass" or "fail",
         "progress" -> list of strings describing how far we got in the test,
         "reason" -> string, which describes the failure or is empty if it's a pass }
    """
    # todo there's a bunch of repeated code for checking the replies from httpx below.
    test_name = os.path.splitext(test_info['file'])[0]
    progress = []

    #### compile the obsidian file in question to yul with a jar of obsidianc
    run_obsidianc = subprocess.run(
        ["java", "-jar", obsidian_jar, "--yul", f"resources/tests/GanacheTests/{test_info['file']}"],
        capture_output=True)
    if not run_obsidianc.returncode == 0:
        return {'result': "fail", 'progress': progress,
                "reason": f"obsidianc run failed with output {run_obsidianc.stderr}"}
    else:
        progress = progress + ["obsidianc compiled obsidian to yul"]

    #### compile the yul to evm with solc
    run_solc = subprocess.run(
        ["docker", "run", "-v", f"{os.getcwd().format()}/{test_name}/:/src", "ethereum/solc:stable",
         "--bin", "--strict-assembly", "--optimize", f"/src/{test_name}.yul"], capture_output=True)
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
    run_ganache = subprocess.Popen(["ganache-cli",
                                    "--verbose",
                                    "--host", ganache_host,
                                    "--port", str(ganache_port),
                                    "--gasLimit", str(hex(test_info.get('gas', defaults['gas']))),
                                    "--accounts", str(test_info.get('numaccts', defaults['numaccts'])),
                                    "--defaultBalanceEther", str(test_info.get('startingeth', defaults['startingeth']))
                                    ], stdout=stdout_redirect)
    progress = progress + [f"started ganache-cli process: {str(run_ganache)}"]

    eth_id = 1  # this can be any number; replies should have the same one. we don't check that.

    #### poll for an account to let it start up
    retries = 100
    json_rpc = "2.0"
    account_reply = polling.poll(
        lambda: httpx.post(ganache_url,
                           json={"jsonrpc": json_rpc, "method": "eth_accounts", "params": [], "id": eth_id}),
        check_success=lambda r: r.status_code == httpx.codes.OK,
        ignore_exceptions=(httpx.ConnectError,),
        step=0.5,
        max_tries=retries
    )

    if not account_reply:
        run_ganache.kill()
        return {'result': "fail", 'progress': progress,
                "reason": f"after {retries} tries, ganache-cli did not produce any account data"}
    else:
        progress = progress + [f"account reply is {str(account_reply.json())}"]

    account_number = account_reply.json()['result'][0]

    #### send a transaction
    transaction_reply = httpx.post(ganache_url, json={"jsonrpc": json_rpc,
                                                      "method": "eth_sendTransaction",
                                                      "params": {
                                                          "from": str(account_number),
                                                          "gas": str(test_info.get('gas', defaults['gas'])),
                                                          "gasPrice": str(
                                                              test_info.get('gasprice', defaults['gasprice'])),
                                                          # the amount of wei to send, which is always nothing
                                                          "value": "0x0",
                                                          "data": f"0x{evm_bytecode}",
                                                      },
                                                      "id": eth_id})

    if not transaction_reply.status_code == httpx.codes.OK:
        run_ganache.kill()
        return {'result': "fail", 'progress': progress,
                "reason": f"posting to eth_sendTransaction got {transaction_reply.status_code} which is not OK"}
    elif "error" in transaction_reply.json().keys():
        run_ganache.kill()
        return {'result': "fail", 'progress': progress,
                "reason": f"posting to eth_sendTransaction returned an error: {str(transaction_reply.json())}"}
    else:
        progress = progress + [f"transaction reply is {str(transaction_reply.json())}"]

    #### warn if there's no expected result because this is as far as we go
    if not test_info['expected']:
        warn(f"no expected result given for test {test_name} so exiting early")
        run_ganache.kill()
        return {'result': "pass", 'progress': progress,
                "reason": "nothing failed; note that no result was checked, though"}

    transaction_hash = transaction_reply.json()['result']

    #### get a transaction receipt to get the contract address
    get_transaction_recipt_reply = httpx.post(ganache_url, json={"jsonrpc": json_rpc,
                                                                 "method": "eth_getTransactionReceipt",
                                                                 "params": [transaction_hash],
                                                                 "id": eth_id})

    if not get_transaction_recipt_reply.status_code == httpx.codes.OK:
        run_ganache.kill()
        return {'result': "fail", 'progress': progress,
                "reason": f"posting to eth_getTransactionReceipt got {get_transaction_recipt_reply.status_code}"
                          "which is not OK"}
    elif not get_transaction_recipt_reply.json()['result']['status'] == "0x1":
        run_ganache.kill()
        return {'result': "fail", 'progress': progress,
                "reason": f"eth_getTransactionReceipt has non-0x1 status {str(get_transaction_recipt_reply.json())}"}
    else:
        progress = progress + [f"getTransactionReceipt reply is {str(get_transaction_recipt_reply.json())}"]

    contract_address = get_transaction_recipt_reply.json()['result']['contractAddress']

    #### use call and the contract address to get the result of the function
    method_name = test_info.get('trans', defaults['trans'])
    method_types = test_info.get('types', defaults['types'])
    method_args = test_info.get('args', defaults['args'])

    keccak_hash = keccak.new(digest_bits=256)
    keccak_hash.update(bytes(method_name + "(" + ",".join(method_types) + ")", "utf8"))
    hash_to_call = keccak_hash.hexdigest()[:8]
    encoded_args = binascii.hexlify(eth_abi.encode_abi(method_types, method_args)).decode()

    call_reply = httpx.post(ganache_url, json={"jsonrpc": json_rpc,
                                               "method": "eth_call",
                                               "params": [
                                                   {"from": account_number,
                                                    "to": contract_address,
                                                    "data": f"0x{hash_to_call}{encoded_args}"
                                                    }, "latest"],
                                               "id": eth_id})

    if not call_reply.status_code == httpx.codes.OK:
        run_ganache.kill()
        return {'result': "fail", 'progress': progress,
                "reason": f"posting to eth_call got {call_reply.status_code} which is not OK"}
    elif 'error' in call_reply.json().keys():
        run_ganache.kill()
        return {'result': "fail", 'progress': progress,
                "reason": f"eth_call reply contains error information {str(call_reply.json())}"}
    else:
        progress = progress + [f"eth_call reply is {str(call_reply.json())}"]

    #### compare the result to the expected answer
    got = twos_comp(int(call_reply.json()['result'], 16), 8 * 32)
    expected = int(test_info['expected'])
    if not got == expected:
        run_ganache.kill()
        return {'result': "fail", 'progress': progress,
                "reason": f"expected {expected} but got {got}"}
    else:
        progress = progress + ["got matched expected"]

    #### decode the logs from the bloom filter, if the test JSON includes a requirement for logs
    getlogs_reply = httpx.post(ganache_url, json={"jsonrpc": json_rpc,
                                                  "method": "eth_getLogs",
                                                  "params": [
                                                      {
                                                          "address": contract_address,
                                                      }
                                                  ],
                                                  "id": eth_id})

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
