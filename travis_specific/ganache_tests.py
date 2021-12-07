import pprint
import sys
import json
import glob
from shutil import which
from termcolor import colored
import subprocess


def warn(s):
    print(colored("WARNING:", 'yellow'), str(s))


def error(s):
    print(colored("ERROR:", 'red'), str(s))
    sys.exit(1)


# todo; grab this off the commandline
verbose = True
testdir = 'resources/tests/GanacheTests/'

if not which("ganache-cli"):
    error("ganache-cli not installed")

# Opening JSON file
f = open(testdir + 'tests.json')
if not f:
    error("could not open tests.json file")
tests_data = json.load(f)
f.close()

# pprint.pprint(tests_data['tests'])

files_with_tests = sorted([test['file'] for test in tests_data['tests']])
files_present = sorted(glob.glob(testdir + '*.obs'))

if not files_present == files_with_tests:
    warn("there are obsidian files present that are not described by the test JSON file:")
