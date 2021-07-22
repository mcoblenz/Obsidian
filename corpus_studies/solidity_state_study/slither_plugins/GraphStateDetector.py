from collections import Counter, deque
from slither.core.solidity_types import UserDefinedType
from slither.core.cfg.node import *
from slither.core.declarations import *

from .ImmutableChecker import ImmutableChecker
from .ContractStateDetector import ContractStateDetector
from .Graph import TransitionGraph
from .EnumGraphInferrer import EnumGraphInferrer
from .BoolGraphInferrer import BoolGraphInferrer

def getEnumStateVar(contract) -> Optional[StateVariable]:
    enum_types_names = [e.canonical_name for e in contract.enums]
    freq = Counter()
    for evs in ContractStateDetector(contract).states_in_contract().values():
        for ev in evs:
            for sv in ev.vars:
                if str(sv.type) in enum_types_names:
                    freq[sv] += 1
    if len(freq) == 0:
        return None
    else:
        return freq.most_common(1)[0][0]

def inferEnumTransitionGraph(contract) -> Optional[Tuple[StateVariable, TransitionGraph]]:
    enum_state_var = getEnumStateVar(contract)
    if enum_state_var is None:
        return None
    else:
        assert(isinstance(enum_state_var.type, UserDefinedType))
        assert(isinstance(enum_state_var.type.type, EnumContract))
        enum_type = enum_state_var.type.type

        enum_explorer = EnumGraphInferrer(contract, enum_state_var, enum_type)
        return (enum_state_var, enum_explorer.inferTransitionGraph())

def getBoolStateVars(contract) -> List[StateVariable]:
    ret = set()
    for evs in ContractStateDetector(contract).states_in_contract().values():
        for ev in evs:
            for sv in ev.vars:
                if isinstance(sv.type, ElementaryType) and sv.type.type == 'bool':
                    ret.add(sv)
    return list(ret)

def inferBoolTransitionGraph(contract, state_var) -> TransitionGraph:
    bool_explorer = BoolGraphInferrer(contract, state_var)
    return bool_explorer.inferTransitionGraph()

# Get all the state variables used in a function.
def state_vars_used_in_function(func: Function) -> Set[StateVariable]:
    return set(func.state_variables_read + func.state_variables_written)

# An GraphStateDetector takes a contract and its transition graph.
# It has methods to compute states that are unreachable from the initial states,
# and also to compute variables which are no longer used after a state transition.
class GraphStateDetector:
    def __init__(self, contract, state_var, graph):
        self.contract: Contract = contract
        self.state_var: StateVariable = state_var
        self.graph: TransitionGraph = graph
        # Dictionary from functions to state variables used in the function/modifiers.
        self.state_vars_used: Dict[Function, Set[StateVariable]] = {f:state_vars_used_in_function(f).union(*(state_vars_used_in_function(m) for m in f.modifiers)) for f in self.contract.functions}

    # Check if a state variable is used in a certain state (i.e., if any function that can be called
    # at that state use this variable)
    def varUsedInState(self, var: StateVariable, state: str) -> bool:
        state_vars_used: Dict[Function, Set[StateVariable]] = \
                {f:state_vars_used_in_function(f).union(*(state_vars_used_in_function(m) for m in f.modifiers)) 
                for f in self.contract.functions}
        for (_, func) in self.graph.adj[state]:
            if var in state_vars_used[func]:
                return True
        return False

    # Get the states reachable from a set of starting states.
    def reachableStates(self, states: Set[str]) -> Set[str]:
        visited = set()
        def visit(s):
            if not s in visited:
                visited.add(s)
                for (t, _) in self.graph.adj[s]:
                    visit(t)
        for s in states: 
            visit(s)
        return visited

    # Get a set of states that are unreachable from the initial states.
    def unreachableStatesFromInit(self) -> Set[str]:
        reachableStates = self.reachableStates(self.graph.initial_states)
        return set(s for s in self.graph.states if s not in reachableStates)

    # Get a list of state variables that will never be used in a state s or any state reachable from s.
    def getUnusedVariables(self) -> Dict[StateVariable, List[str]]:
        ret = {s:[] for s in self.graph.states}
        q = deque(self.graph.initial_states)
        for s in self.graph.states:
            reachableStates = self.reachableStates({s})
            for var in self.contract.state_variables:
                if var != self.state_var and not var.is_constant and all(not self.varUsedInState(var, s) for s in reachableStates):
                    ret[s].append(var)
        return ret

# Returns a string containing information about the contract and the inferred
# state graph in a readable format.
def enumStateInfo(contract, state_var, graph) -> str:
    detector = GraphStateDetector(contract, state_var, graph)
    message = "Contract %s\n" % contract
    message += "Identified enum: %s\n" % state_var.canonical_name
    message += "Identified states: %s\n" % graph.states
    message += "Initial states: %s\n" % graph.initial_states
    message += str(graph) + "\n"
    message += "Unreachable states: %s\n" % [s for s in detector.unreachableStatesFromInit()]
    message += "Unused variables:\n"
    unused = detector.getUnusedVariables()
    for (s,vars) in unused.items():
        message += "%s %s\n" % (s, [v.name for v in vars])
    return message

def boolStateInfo(contract, state_var, graph):
    detector = GraphStateDetector(contract, state_var, graph)
    ret = {}
    ret['Contract'] = contract.name
    ret['Statevar'] = state_var.name
    ret['Initial states'] = graph.initial_states
    ret['Graph'] = graph
    ret['Unreachable states'] = detector.unreachableStatesFromInit()
    unused = detector.getUnusedVariables()
    immutable_checker = ImmutableChecker(contract)
    for var in contract.state_variables:
        if immutable_checker.could_be_immutable(var) or all(var in unused[s] for s in graph.states):
            for s in graph.states:
                if var in unused[s]:
                    unused[s].remove(var)
    ret['Unused variables'] = {k:v for (k,v) in unused.items() if len(v) > 0}
    return ret

def formatBoolInfo(info) -> str:
    message = "Contract: %s\n" % info['Contract']
    message += "State variable: %s\n" % info['Statevar']
    message += "Initial states: %s\n" % info['Initial states']
    message += "Graph:\n%s\n" % info['Graph']
    message += "Unreachable states: %s\n" % info['Unreachable states']
    message += "Unused variables:\n"
    for (s,vars) in info['Unused variables'].items():
        message += "%s %s\n" % (s, [v.name for v in vars])
    return message
