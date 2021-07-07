from slither.core.expressions import *
from slither.core.cfg.node import *
from slither.core.declarations.solidity_variables import SOLIDITY_VARIABLES, SOLIDITY_VARIABLES_COMPOSED
from slither.detectors.functions.modifier import is_revert
from slither.analyses.data_dependency.data_dependency import *

# Solidity variables that are used as state variables instead of 
# local variables, and thus must be explicitly checked for when
# checking for state usage.
SOLIDITY_VARIABLE_GLOBAL_STATES = ["now", "this", "block.number", "block.timestamp"]
# Solidity variables that are not allowed to appear in a stateful expression.
SOLIDITY_VARIABLE_GLOBAL_NOTSTATES = ["msg.sender", "tx.origin", "msg.value"]

# Checks whether a throw/assert is reachable from the current node.
def can_reach_revert(node) -> bool:
    return any(is_revert(e) for e in recheable(node))

# This class determines whether a contract is using states, and if so, will
# provide additional information about the states.
# The function used by the hasstate detector is is_stateful_contract().
class ContractStateDetector: 
    def __init__(self, contract: Contract):
        self.contract = contract
        self.enum_names = [e.name for e in contract.enums]
        self.state_vars = contract.state_variables
        self.nonconstant_vars = [sv for sv in self.state_vars if not sv.is_constant]
        # Getters are functions that return values that flow from fields.
        # self.getters is a dictionary from function names (strings) to fields that appear in them
        self.getters = {}
        for m in self.contract.functions:
            fields = self.fields_in_getter(m)
            if fields is not None:
                self.getters[str(m)] = fields

    # Gets all variables used in an expression, returning whether the expression
    # is a valid condition for a state check.
    # In particular, all calls in the expression must be getters defined by the contract.
    # Returns a boolean, which indicates whether the expression is valid (does not contain getters
    # or unexpected expressions), and a set of variables used.
    def gather_vars(self, exp: Expression) -> Tuple[bool, Set[Variable]]:
        # Merge the two return values; the result is valid if both a and b are valid.
        def merge(a: Tuple[bool, Set[Variable]], b: Tuple[bool, Set[Variable]]) -> Tuple[bool, Set[Variable]]:
            return (a[0] and b[0], a[1] | b[1])
        if isinstance(exp, Identifier):
            if isinstance(exp.value, Variable) or isinstance(exp.value, SolidityVariable):
                return (True,{exp.value})
            else:
                return (True,set())
        elif isinstance(exp, CallExpression):
            if str(exp.called) in self.getters:
                ret = (True,self.getters[str(exp.called)])
                for arg in exp.arguments:
                    ret = merge(ret,self.gather_vars(arg))
                return ret
            else: return (False,set())
        elif isinstance(exp, UnaryOperation):
            return self.gather_vars(exp.expression)
        elif isinstance(exp, BinaryOperation):
            return merge(self.gather_vars(exp.expression_left),self.gather_vars(exp.expression_right))
        elif isinstance(exp, Literal):
            return (True,set())
        elif isinstance(exp, IndexAccess):
            return merge(self.gather_vars(exp.expression_left),self.gather_vars(exp.expression_right))
        elif isinstance(exp, MemberAccess):
            return self.gather_vars(exp.expression)
        elif isinstance(exp, TupleExpression):
            # There should be exactly one element in the tuple 
            #     (that is, they should act as parenthesis).
            # Comparing tuples for equality as a Solidity language feature
            #     does not exist at the time of writing.
            return self.gather_vars(exp.expressions[0])
        elif isinstance(exp, TypeConversion):
            return self.gather_vars(exp.expression)
        return (False,set())

    # Determines if an expression is constant, 
    # meaning the only variables used are constant variables.
    def is_constant_expr(self, exp: Expression):
        (valid, vars) = self.gather_vars(exp)
        if valid:
            for v in vars:
                if isinstance(v, SolidityVariable):
                    return False
                elif not v.is_constant:
                    return False
            return True
        else: return False

    # Check if a function is a getter, and if so, returns a list of state variables used.
    # Otherwise returns None
    def fields_in_getter(self, func: Function) -> Optional[Set[Variable]]:
        # Check if a node is a return statement.
        # Returns None if the return statement does not rely on state variables,
        # and returns empty set if the node is not a return statement.
        def fields_in_getter_node(node: Node) -> Set[Variable]:
            if (node.type == NodeType.RETURN) and node.expression is not None:
                # There is a potential bug here: at this point, getters is still being initialized, but 
                # gather_vars assumes getters is initialized to check if any function calls are to getters.
                # This may be resolved by iterating over the functions according to a topological ordering of function dependencies.
                # On the other hand, it's not clear how often a getter function actually calls other getter functions.
                (valid, vars) = self.gather_vars(node.expression)
                if valid:
                    return vars
                else: return None
            return set()
        if func.view:
            vars: Set[Variable] = set()
            for node in func.nodes:
                node_vars = fields_in_getter_node(node)
                if node_vars is None:
                    return None
                else:
                    vars |= node_vars
            return vars
        return None

    # Checks if the node makes a stateful check
    # Returns a set of state variables used, or an empty set if none are found
    # constant_parameters are the variables (in modifiers) which we know to be constant values.
    def statevars_in_node(self, node: Node, func: Function, constant_parameters: Set[Variable] = None) -> Set[StateVariable]:
        if constant_parameters is None:
            constant_parameters = set()
        # Return a set of all (nonconstant) state vars that the variable is dependent on
        def state_vars_used(var: Variable) -> Set[StateVariable]:
            if var in self.state_vars:
                if var in self.nonconstant_vars:
                    return {var}
                else:
                    return set()
            else:
                # If var is dependent on itself, remove it to stop infinite recursion
                next_vars = get_dependencies(var, func) - {var}
                return set().union(*map(state_vars_used, next_vars))
        # Check if a variable is dependent only on state variables
        def is_dependent_on_state_vars(var: Variable):
            if var in func.parameters or var.name in SOLIDITY_VARIABLE_GLOBAL_NOTSTATES:
                return False
            # If var is dependent on itself, remove it to stop infinite recursion
            next_vars = get_dependencies(var, func) - {var}
            return var in self.state_vars or all(map(is_dependent_on_state_vars, next_vars))
        # Check if a variable is dependent on at least one nonconstant state variable
        def is_dependent_on_nonconstant(var: Variable):
            # If var is dependent on itself, remove it to stop infinite recursion
            next_vars = get_dependencies(var, func) - {var}   
            return var in self.nonconstant_vars or any(map(is_dependent_on_nonconstant, next_vars))
        def is_stateful_expression(expression: Expression) -> Set[Variable]:
            (valid, vars) = self.gather_vars(expression)
            if valid:
                vars -= constant_parameters
                # returns set of used state variables if all variables are either dependent only on state variables or are constant
                # AND at least one of them is nonconstant (or dependent on nonconstant variables)
                if all(is_dependent_on_state_vars(var) or str(var) in SOLIDITY_VARIABLE_GLOBAL_STATES for var in vars) and \
                   any(is_dependent_on_nonconstant(var) for var in vars):
                    return set().union(*map(state_vars_used, vars))
                else: return set()
            else: return set()
            
        expression = None
        if node.contains_require_or_assert() :
            #require and assert both only have one argument.
            expression = node.expression.arguments[0]
        # If the node is an if expression, we check that either branch leads to a throw/revert.
        elif node.contains_if(include_loop=False) and can_reach_revert(node): 
            expression = node.expression
        
        if expression is not None:
            return is_stateful_expression(expression)
        return set()

    # Checks if the modifier has a stateful check
    # Returns a set of state variables used, or an empty set if none are found
    def statevars_in_modifier(self, func: Function, constant_parameters: Set[Variable]) -> Set[StateVariable]:
        return set().union(*map(lambda n: self.statevars_in_node(n, func, constant_parameters), func.nodes))

    # Checks if the function  or any called modifiers has a stateful check
    # Returns a set of state variables used, or an empty set if none are found
    def statevars_in_function(self, func: Function) -> Set[StateVariable]:
        ret = set()
        # Check modifiers, ignoring parameters whose inputs are constant values.
        # For instance, in this example, the modifier inStage is being called with a constant value Stage.NotStarted,
        # so when checking the modifier, the require statement is flagged as evidence of state.
        #   Stage stage;
        #   modifier inStage(Stage _stage) {
        #     require(stage == _stage);
        #     _;
        #   }
        #   function startGame(Stage s) public inStage(Stage.NotStarted) {
        #     stage = Stage.InProgress;
        #   }
        for m in func.modifiers:
            params = m.parameters
            # Find the internal call to the modifier and get the list of arguments.
            args = next(c.arguments for c in func.calls_as_expressions if isinstance(c, CallExpression) and str(c.called) == m.name)
            assert(args != None)
            assert(len(args) == len(params))

            constant_parameters = set()
            for (arg, param) in zip(args, params):
                if self.is_constant_expr(arg):
                    constant_parameters.add(param)
            ret |= self.statevars_in_modifier(m, constant_parameters)

        ret |= set().union(*map(lambda n: self.statevars_in_node(n, func), func.nodes))
        return ret

    # Checks if the contract has a function or modifier that makes a stateful check.
    def is_stateful_contract(self) -> bool:
        ret = set()
        for f in self.contract.functions:
            # TODO: Return this information for the detector
            vars = self.statevars_in_function(f)
            print("Function %s: fields: %s" % (f.name, [v.name for v in vars]))
            ret |= vars
        return len(ret) > 0
