from ContractStateDetector import ContractStateDetector
from ..tests.set_solc_version import set_version
import glob
import pytest
from slither import Slither

# To run tests, run python3 -m pytest run_tests.py

# We assume we are checking the last contract in the file.
# For those in the state directory, we expect the last contract to be detected with state.
# For those in the nostate directory, we expect the last contract to not be detected with state.

state_files = glob.glob("tests/state/*.sol")
@pytest.mark.parametrize('f', state_files)
def test_state(f):
    set_version(f)
    slither = Slither(f)
    c = slither.contracts[-1]
    assert(ContractStateDetector(c).is_stateful_contract())

nostate_files = glob.glob("tests/nostate/*.sol")
@pytest.mark.parametrize('f', nostate_files)
def test_nostate(f):
    set_version(f)
    slither = Slither(f)
    c = slither.contracts[-1]
    assert(not ContractStateDetector(c).is_stateful_contract())
