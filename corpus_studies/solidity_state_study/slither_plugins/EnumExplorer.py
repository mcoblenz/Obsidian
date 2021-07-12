from slither.core.expressions import *
from slither.core.cfg.node import *
from slither.core.declarations import *
from slither.detectors.functions.modifier import is_revert
from .Graph import Edge, TransitionGraph

# Checks if the expression is an enum constant of the type enum_type.
def expr_is_enum_val(exp: Expression, enum_type: EnumContract) -> bool:
    return isinstance(exp, MemberAccess) and isinstance(exp.expression, Identifier) and \
        exp.expression.value == enum_type

# Checks if the expression is equal to the variable var.
def expr_is_var(exp: Expression, var: Variable) -> bool:
    return isinstance(exp, Identifier) and exp.value == var

class EnumExplorer:
    def __init__(self, 
                contract: Contract,
                state_var: StateVariable, 
                enum_type: EnumContract):
        self.contract = contract
        self.state_var = state_var
        self.all_states = set(enum_type.values)
        self.enum_type = enum_type

    def inferTransitionGraph(self) -> TransitionGraph:
        # Find the variable constructor, if it exists (where fields are initialized when they are declared).
        constructor_vars = next((f for f in self.contract.functions if f.is_constructor_variables), None)
        # Find the contract's constructor, if it exists.
        # A contract can have a most one constructor, not counting inherited constructors.
        # For a contract C, we are looking for a function with canonical name "C.constructor()"
        # or "C.C()", depending on the version of Solidity.
        constructor = next((f for f in self.contract.functions if f.is_constructor and f.canonical_name.startswith(self.contract.name)), None)
        initial_states = self.find_initial_states(constructor_vars, constructor)

        graph = TransitionGraph(set(self.enum_type.values), initial_states)

        for func in self.contract.functions:
            if not (func.is_constructor or func.is_constructor_variables):
                edges = self.exploreFunction(func)
                for edge in edges:
                    graph.addEdge(edge.s_from, edge.s_to, edge.label)
        return graph

    def find_initial_states(self, constructor_vars, constructor) -> Set[str]:
        initial_states = {self.enum_type.values[0]}
        if constructor_vars is not None:
            edges = self.exploreFunction(constructor_vars)
            initial_states = {edge.s_to for edge in edges if edge.s_from in initial_states}
        if constructor is not None:
            edges = self.exploreFunction(constructor)
            initial_states = {edge.s_to for edge in edges if edge.s_from in initial_states}
        return initial_states

    def exploreFunction(self, func: Function) -> List[Edge]:
        self.edges = set()
        self.func = func
        if func.entry_point is not None:
            self.exploreNode(func.entry_point, self.all_states, {None})
            return list(self.edges)
        else:
            return [Edge(s,s,func) for s in self.all_states]

    def completeCodePath(self, starting_states: Set[str], ending_states: Set[Optional[str]]):
        for start in starting_states:
            for end in ending_states:
                if end is None: 
                    # A value of none means the value of the state variable has not changed
                    self.edges.add(Edge(start, start, self.func))
                else:
                    self.edges.add(Edge(start, end, self.func))

    # Explore an execution path starting at the given node, maintaining the set of
    # possible starting and ending states.
    # Once we have reached the end of an execution path, call completeCodePath to add the 
    # starting and ending states as edges to the graph.
    def exploreNode(self, 
                    node: Node, 
                    starting_states: Set[str], 
                    ending_states: Set[Optional[str]], 
                    subs: Dict[Variable, Expression] = None,
                    level: int = 0) -> Set[str]:
        if subs is None: subs = {}
        new_starting_states = self.mergeInfo(node, starting_states, ending_states, subs, level)
        new_ending_states = self.mergeEndingStates(node, ending_states, subs)
        
        if is_revert(node):
            return set()

        if len(node.sons) == 0:
            if level == 0:
                self.completeCodePath(new_starting_states, new_ending_states)
            return new_starting_states
        elif len(node.sons) == 1:
            return self.exploreNode(node.sons[0], new_starting_states, new_ending_states, subs, level)
        else:
            # If we reach an if statement, add the condition/negated condition to the
            # list of assumptions for the respective branches, refining the set of 
            # possible starting states.
            if node.type == NodeType.IF:
                starting_states_true = new_starting_states & self.possibleStates(node.expression, subs)
                res_true = self.exploreNode(node.sons[0], starting_states_true, new_ending_states, subs, level)
                negated_exp = UnaryOperation(node.expression, UnaryOperationType.BANG)
                starting_states_false = new_starting_states & self.possibleStates(negated_exp, subs)
                res_false = self.exploreNode(node.sons[1], starting_states_false, new_ending_states, subs, level)
                return res_true | res_false
            elif node.type == NodeType.IFLOOP:
                # Ignore loop body and go to ENDLOOP node
                return self.exploreNode(node.son_false, new_starting_states, new_ending_states, subs, level)
            else:
                raise Exception("Unexpected node: %s" % node)
                # TODO: Handle other node types
        
    # Given a node and a set of possible states, return a refined set of possible initial states
    # If the node is a require/assert, return the set of states that can make the condition true
    # If the node is a modifier call, return the preconditions for the function/modifier.
    def mergeInfo(self, node: Node, starting_states: Set[str], ending_states: Set[Optional[str]], subs: Dict[Variable, Expression], level: int) -> Set[str]:
        if node.contains_require_or_assert():
            #require and assert both only have one argument.
            states = self.possibleStates(node.expression.arguments[0], subs)
            return starting_states & states
        elif isinstance(node.expression, CallExpression) and \
             isinstance(node.expression.called, Identifier) and \
             isinstance(node.expression.called.value, Modifier):
            func = node.expression.called.value
            params = func.parameters
            arguments = node.expression.arguments
            assert(len(params) == len(arguments))
            # may need to merge the two subs somehow
            subs: Dict[LocalVariable, Expression] = dict(zip(params, arguments))
            # print("Call %s: %s" % (func, [str(x) for x in arguments]))
            return starting_states & self.exploreNode(func.entry_point, starting_states, ending_states, subs, level+1)
        else:
            return starting_states.copy()

    # Look for expressions assigning a constant enum value to the state var.
    # If the RHS of an assignment is not recognized to be a constant value, assume the variable could hold any value.
    def mergeEndingStates(self, 
                          node: Node, 
                          ending_states: Set[Optional[str]], 
                          subs: Dict[Variable, Expression]) -> Set[Optional[str]]:
        if isinstance(node.expression, AssignmentOperation) and \
           node.expression.type == AssignmentOperationType.ASSIGN and \
           expr_is_var(node.expression.expression_left, self.state_var):
            rhs = node.expression.expression_right
            if expr_is_enum_val(rhs, self.enum_type):
                assert(isinstance(rhs, MemberAccess))
                return {rhs.member_name}
            else:
                return self.all_states
        else:
            return ending_states.copy()
    
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
    # In particular, we look for expressions comparing the state var to a constant 
    # enum value, or any boolean combination of such expressions.
    def _possibleStates(self, exp: Expression, subs: Dict[Variable, Expression]) -> Optional[Set[str]]:
        if subs is None: subs = {}
        if isinstance(exp, UnaryOperation) and exp.type == UnaryOperationType.BANG: # !; Take complement
            ret = self._possibleStates(exp.expression, subs)
            if ret is None:
                return None
            else:
                return self.all_states - ret
        elif isinstance(exp, BinaryOperation):
            if exp.type == BinaryOperationType.ANDAND or exp.type == BinaryOperationType.OROR:
                left = self._possibleStates(exp.expression_left, subs)
                right = self._possibleStates(exp.expression_right, subs)
                if left is None:
                    return right
                elif right is None:
                    return left
                else:
                    if exp.type == BinaryOperationType.ANDAND: # &&; Take intersection
                        return left & right
                    elif exp.type == BinaryOperationType.OROR: # ||: Take union
                        return left | right
            elif exp.type == BinaryOperationType.EQUAL or exp.type == BinaryOperationType.NOT_EQUAL:
                exp_left = exp.expression_left
                exp_right = exp.expression_right
                # If the right expression is our state variable, swap left and right.
                # This reduces the number of checks we have to do.
                if expr_is_var(exp_right, self.state_var):
                    exp_left, exp_right = exp_right, exp_left
                if expr_is_var(exp_left, self.state_var):
                    # Check if the RHS is either an enum value, or a variable which we know is a constant enum value (from the subs dictionary)
                    if isinstance(exp_right, Identifier) and exp_right.value in subs:
                        exp_right = subs[exp_right.value]
                    # Check if exp_right is an enum value.
                    if expr_is_enum_val(exp_right, self.enum_type):
                        if exp.type == BinaryOperationType.EQUAL:
                            return {exp_right.member_name}
                        elif exp.type == BinaryOperationType.NOT_EQUAL:
                            return self.all_states - {exp_right.member_name}
        return None
