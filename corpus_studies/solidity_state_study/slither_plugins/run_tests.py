from hasstate import ContractStateDetector
import glob
from slither import Slither
from slither.detectors.state.hasstate import HasState

# For those in the state directory, we expect at least one contract to be detected with state.
# For those in the nostate directory, we expect none of the contracts to b  e detected with state.
def test_state():
    failed = []
    files = [f for f in glob.glob("tests/state/*.sol")]
    for f in files:
        slither = Slither(f)
        if not any(ContractStateDetector(c).is_stateful_contract() for c in slither.contracts):
            failed.append(f)
    return failed

def test_nostate():
    failed = []
    files = [f for f in glob.glob("tests/nostate/*.sol")]
    for f in files:
        slither = Slither(f)
        if any(ContractStateDetector(c).is_stateful_contract() for c in slither.contracts):
            failed.append(f)
    return failed

if __name__ == '__main__':
    print("Testing contracts (expecting state):")
    should_detect = test_state()
    if len(should_detect) == 0:
        print("Passed")
    else:
        print("Failed: ", should_detect)
    
    print("Testing contracts (expecting no state):")
    should_detect = test_nostate()
    if len(should_detect) == 0:
        print("Passed")
    else:
        print("Failed: ", should_detect)
