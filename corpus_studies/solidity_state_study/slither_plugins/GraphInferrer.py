from abc import ABC, abstractmethod
from slither.core.expressions import *
from slither.core.cfg.node import *
from slither.core.declarations import *
from slither.detectors.functions.modifier import is_revert
from .Graph import Edge, TransitionGraph

# Checks if the expression is equal to the variable var.
def expr_is_var(exp: Expression, var: Variable) -> bool:
    return isinstance(exp, Identifier) and exp.value == var

class GraphInferrer(ABC):
    def __init__(self, 
                contract: Contract,
                state_var: StateVariable):
        self.contract = contract
        self.state_var = state_var
    
    @property
    @abstractmethod
    def all_states(self) -> Set[str]:
        pass

    @property
    @abstractmethod
    def uninitialized_state(self) -> str:
        pass

    def inferTransitionGraph(self) -> TransitionGraph:
        # Find the variable constructor, if it exists (this is where fields are initialized when they are declared).
        constructor_vars = next((f for f in self.contract.functions if f.is_constructor_variables), None)
        # Find the contract's constructor, if it exists.
        # A contract can have a most one constructor, not counting inherited constructors.
        # For a contract C, we are looking for a function with canonical name "C.constructor()"
        # or "C.C()", depending on the version of Solidity.
        constructor = next((f for f in self.contract.functions if f.is_constructor and f.canonical_name.startswith(self.contract.name)), None)
        initial_states = self.find_initial_states(constructor_vars, constructor)

        graph = TransitionGraph(self.all_states, initial_states)

        for func in self.contract.functions:
            if not (func.is_constructor or func.is_constructor_variables):
                edges = self.inferEdgesFromFunction(func)
                for edge in edges:
                    graph.addEdge(edge.s_from, edge.s_to, edge.label)
        return graph

    def find_initial_states(self, constructor_vars, constructor) -> Set[str]:
        initial_states = {self.uninitialized_state}
        if constructor_vars is not None:
            edges = self.inferEdgesFromFunction(constructor_vars)
            initial_states = {edge.s_to for edge in edges if edge.s_from in initial_states}
        if constructor is not None:
            edges = self.inferEdgesFromFunction(constructor)
            initial_states = {edge.s_to for edge in edges if edge.s_from in initial_states}
        return initial_states

    def inferEdgesFromFunction(self, func: Function) -> List[Edge]:
        self.edges = set()
        self.func = func
        if func.entry_point is not None:
            self.inferEdgesFromNode(func.entry_point, self.all_states, {None})
            return list(self.edges)
        else:
            return [Edge(s,s,func) for s in self.all_states]

    # Add edges to the graph from connecting the possible starting states to the possible ending states.
    def addEdgesFromCodePath(self, 
                             starting_states: Set[str], 
                             ending_states: Set[Optional[str]]):
        for start in starting_states:
            for end in ending_states:
                if end is None: 
                    # A value of none means the value of the state variable has not changed
                    self.edges.add(Edge(start, start, self.func))
                else:
                    self.edges.add(Edge(start, end, self.func))

    # Explore an execution path starting at the given node, maintaining the set of
    # possible starting and ending states.
    # Once we have reached the end of an execution path, call addEdgesFromCodePath to add the 
    # starting and ending states as edges to the graph.
    def inferEdgesFromNode(self, 
                    node: Node, 
                    starting_states: Set[str], 
                    ending_states: Set[Optional[str]], 
                    subs: Dict[Variable, Expression] = None,
                    level: int = 0) -> Set[str]:
        if subs is None: subs = {}
        new_starting_states = self.mergeInfo(node, starting_states, ending_states, subs, level)
        new_ending_states = self.mergeEndingStates(node, ending_states)
        
        if is_revert(node):
            return set()

        if len(node.sons) == 0:
            if level == 0:
                self.addEdgesFromCodePath(new_starting_states, new_ending_states)
            return new_starting_states
        elif len(node.sons) == 1:
            return self.inferEdgesFromNode(node.sons[0], new_starting_states, new_ending_states, subs, level)
        else:
            # If we reach an if statement, add the condition/negated condition to the
            # list of assumptions for the respective branches, refining the set of 
            # possible starting states.
            if node.type == NodeType.IF:
                starting_states_true = new_starting_states & self.possibleStates(node.expression, subs)
                res_true = self.inferEdgesFromNode(node.sons[0], starting_states_true, new_ending_states, subs, level)
                negated_exp = UnaryOperation(node.expression, UnaryOperationType.BANG)
                starting_states_false = new_starting_states & self.possibleStates(negated_exp, subs)
                res_false = self.inferEdgesFromNode(node.sons[1], starting_states_false, new_ending_states, subs, level)
                return res_true | res_false
            elif node.type == NodeType.IFLOOP:
                # Ignore loop body and go to ENDLOOP node
                return self.inferEdgesFromNode(node.son_false, new_starting_states, new_ending_states, subs, level)
            else:
                raise Exception("Unexpected node: %s" % node)
                # TODO: Handle other node types
        
    # Given a node and a set of possible states, return a refined set of possible initial states
    # If the node is a require/assert, return the set of states that can make the condition true
    # If the node is a modifier call, return the preconditions for the function/modifier.
    @abstractmethod
    def mergeInfo(self, 
                  node: Node, 
                  starting_states: Set[str], 
                  ending_states: Set[Optional[str]], 
                  subs: Dict[Variable, Expression], 
                  level: int) -> Set[str]:
        pass

    # Look for expressions assigning a constant enum value to the state var.
    # If the RHS of an assignment is not recognized to be a constant value, assume the variable could hold any value.
    @abstractmethod
    def mergeEndingStates(self, 
                          node: Node, 
                          ending_states: Set[Optional[str]]) -> Set[Optional[str]]:
        pass
    
    # Call the helper function _possibleStates, returning the set of all states if the result is None
    def possibleStates(self, exp: Expression, subs: Optional[Dict[Variable, Expression]]) -> Set[str]:
        states = self._possibleStates(exp, subs)
        if states is not None:
            return states
        else:
            return self.all_states

    # Given an expression (and an optional list of argument values for modifiers),
    # return a set of state values that will make the expression true, or None if
    # the expression is not relevant.
    @abstractmethod
    def _possibleStates(self, exp: Expression, subs: Dict[Variable, Expression]) -> Optional[Set[str]]:
        pass
