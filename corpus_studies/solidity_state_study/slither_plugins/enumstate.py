from slither.detectors.abstract_detector import AbstractDetector, DetectorClassification
from .EnumStateDetector import EnumStateDetector, inferTransitionGraph

# Class implemented as a detector plugin for Slither. This detector detects,
# for the given contracts, which contracts have an abstract state controlled
# by a single enum value. It will report the states and transitions, and also
# unreachable states and state variables which will never be used in a certain state.
class EnumState(AbstractDetector):

    STATEFUL_MESSAGE = "%s is stateful\n"

    # Variables declared for use in Slither
    ARGUMENT = 'enumstate'
    HELP = 'Help printed by slither'
    IMPACT = DetectorClassification.INFORMATIONAL
    CONFIDENCE = DetectorClassification.HIGH

    WIKI = 'ENUM STATE CHECK'

    WIKI_TITLE = 'Enum State'
    WIKI_DESCRIPTION = "Detects if a contract's state is controlled by a single enum value"
    WIKI_EXPLOIT_SCENARIO = '''
    contract Foo {
        enum State {A, B, C, D}
        State public state;
        uint public x;
        
        constructor() {
            state = State.B;
            x = 0;
        }

        modifier inState(State s) {
            require(state == s);
            _;
        }

        function goToB() public {
            require(state == State.A || state == State.C);
            state = State.B;
        }

        function goToC() public inState(State.B) {
            state = State.C;
        }
    }

    Here is the contract's transition graph:
    A --> B <--> C

    D

    Initial state: B
    Unreachable states: A, D
    '''

    WIKI_RECOMMENDATION = 'This is just a check, it does not indicate an error'
    
    # Function called by Slither framework. Checks all contracts in scope of 
    # the detector and gives the result of the check on those contracts.
    def _detect(self):
        stateful_contracts = []
        for c in self.contracts:
            inferGraph = inferTransitionGraph(c)
            if inferGraph is not None:
                (state_var, graph) = inferGraph
                detector = EnumStateDetector(c, state_var, graph)
                message = "Contract %s\n" % c
                message += "Identified enum: %s\n" % state_var.canonical_name
                message += "Identified states: %s\n" % graph.states
                message += "Initial states: %s\n" % graph.initial_states
                message += str(graph) + "\n"
                message += "Unreachable states: %s\n" % [s for s in detector.unreachableStatesFromInit()]
                message += "Unused variables:\n"
                unused = detector.getUnusedVariables()
                for (s,vars) in unused.items():
                    message += "%s %s\n" % (s, [v.name for v in vars])
                
                stateful_contracts.append(message)

        if stateful_contracts:
            return [self.generate_result(stateful_contracts, additional_fields=None)]
        return []
