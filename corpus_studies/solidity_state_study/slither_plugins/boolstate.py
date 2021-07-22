from slither.detectors.abstract_detector import AbstractDetector, DetectorClassification
from .GraphStateDetector import *

# Class implemented as a detector plugin for Slither. This detector detects,
# for the given contracts, which contracts have an abstract state controlled
# by a single enum value. It will report the states and transitions, and also
# unreachable states and state variables which will never be used in a certain state.
class BoolState(AbstractDetector):

    STATEFUL_MESSAGE = "%s is stateful\n"

    # Variables declared for use in Slither
    ARGUMENT = 'boolstate'
    HELP = 'Help printed by slither'
    IMPACT = DetectorClassification.INFORMATIONAL
    CONFIDENCE = DetectorClassification.HIGH

    WIKI = 'BOOL STATE CHECK'

    WIKI_TITLE = 'Bool State'
    WIKI_DESCRIPTION = "Detects if a contract's state is controlled by a boolean which is deactivated"
    WIKI_EXPLOIT_SCENARIO = '''
    contract Deactivation {
        bool stopped;
        uint public x;
        uint y;
        
        constructor() {
            x = 5;
        }

        function stop() public {
            require(stopped == false);
            stopped = true;
        }

        function addToX() public {
            require(!stopped);
            y++;
            x += y;
        }
    }

    Here, the function addToX is deactivated, meaning that after stop() is called, addToX and the field y will never be used again.
    '''

    WIKI_RECOMMENDATION = 'This is just a check, it does not indicate an error'
    
    # Function called by Slither framework. Checks all contracts in scope of 
    # the detector and gives the result of the check on those contracts.
    def _detect(self):
        stateful_contracts = []
        for c in self.contracts:
            bool_vars = getBoolStateVars(c)
            for state_var in bool_vars:
                graph = inferBoolTransitionGraph(c, state_var)
                message = formatBoolInfo(boolStateInfo(c, state_var, graph))
                stateful_contracts.append(message)

        if stateful_contracts:
            return [self.generate_result(stateful_contracts, additional_fields=None)]
        return []
