from slither.detectors.abstract_detector import AbstractDetector, DetectorClassification
from slither.core.expressions import *
from slither.core.cfg.node import *
from slither.core.declarations.solidity_variables import SOLIDITY_VARIABLES,SOLIDITY_VARIABLES_COMPOSED
from slither.detectors.functions.modifier import is_revert
from functools import reduce

# Solidity variables that are used as state variables instead of 
# local variables, and thus must be explicitly checked for when
# checking for state usage.
SOLIDITY_VARIABLE_WHITELIST = ["now", "this", "block.number", "block.timestamp"]

# This is a function used to check for state in multiple plugins, 
# and thus belongs on this level for easy code reuse.
#
# It takes a Solidity AST used inside of a conditional check, exp, 
# and traverses it to find all of the variables used in the check.
# It converts the variables to strings and returns them in a set.
def get_vars_used(exp, enum_names) :
    if isinstance(exp, Identifier) :
        return {str(exp)}
    elif isinstance(exp, CallExpression) :
        vars = [get_vars_used(arg, enum_names) for arg in exp.arguments]
        return reduce(lambda s,x : s.union(x), vars, set())
    elif isinstance(exp, UnaryOperation) :
        return get_vars_used(exp.expression, enum_names)
    elif isinstance(exp, BinaryOperation) :
        return (get_vars_used(exp.expression_left, enum_names)
                    .union(get_vars_used(exp.expression_right, enum_names))
                )
    elif isinstance(exp, Literal) :
        return set()
    elif isinstance(exp, IndexAccess) :
        return (get_vars_used(exp.expression_left, enum_names)
                    .union(get_vars_used(exp.expression_right, enum_names))
                )
    elif isinstance(exp, MemberAccess) :
        # If the expression is an enum value that is defined in the contract or a parent
        # contract, do not include it in the set of variables.
        if (isinstance(exp.expression, Identifier) and str(exp.expression) in enum_names):
            return set()
        return {str(exp)}
    elif isinstance(exp, TupleExpression) :
        # There should be exactly one element in the tuple 
        #     (that is, they should act as parenthesis).
        # Comparing tuples for equality as a Solidity language feature
        #     does not exist at the time of writing.
        return get_vars_used(exp.expressions[0], enum_names)
    return set()

# Checks whether a throw/assert is reachable from the current node.
def can_reach_revert(node):
    return any(is_revert(e) for e in recheable(node))

# Class implemented as a detector plugin for Slither. This detector detects,
# for the given contracts, which contracts have state checks and 
# thus are stateful
class HasState(AbstractDetector):

    STATEFUL_MESSAGE = "%s is stateful\n"

    # Variables declared for use in Slither
    ARGUMENT = 'hasstate'
    HELP = 'Help printed by slither'
    IMPACT = DetectorClassification.INFORMATIONAL
    CONFIDENCE = DetectorClassification.HIGH

    WIKI = 'STATE TEST'

    WIKI_TITLE = 'TODO'
    WIKI_DESCRIPTION = 'Detects whether or not a contract is stateful'
    WIKI_EXPLOIT_SCENARIO = '''
    contract Pausable {
        event Pause();
        event Unpause();

        bool public paused = false;

        modifier whenNotPaused() {
            require(!paused);
            _;
        }

        modifier whenPaused {
            require(paused);
            _;
        }

        function pause() whenNotPaused returns (bool) {
            paused = true;
            Pause();
            return true;
        }

        function unpause() whenPaused returns (bool) {
            paused = false;
            Unpause();
            return true;
        }
    }
    
    In the above contract, there are two states checks, 
    "require(paused)" and "require(!paused)". This contract is 
    therefore stateful, and the detector will print out:
    %s
    ''' % (STATEFUL_MESSAGE % "Pausable")

    WIKI_RECOMMENDATION = 'This is just a check, it does not indicate an error'

    # Checks if any contracts that contract inherits were already determined 
    # to be stateful. Returns the truth value of that check.
    def inherited_state(self, contract, stateful) :
        inherited = [str(c) for c in contract.inheritance]
        return bool([elem for elem in inherited if elem in stateful])
    
    # Checks if the node makes a stateful check, and gives the result of
    # that check
    def is_stateful_node(self, node, state_vars, enum_names) :
        str_vars = [str(sv) for sv in state_vars]
        constant_vars = [str(sv) for sv in state_vars if sv.is_constant]
        nonconstant_vars = [str(sv) for sv in state_vars if not sv.is_constant]
        argument = None
        if node.contains_require_or_assert() :
            #require and assert both only have one argument.
            argument = node.expression.arguments[0]
        # If the node is an if expression, we check that either branch leads to a throw/revert.
        elif node.contains_if(include_loop=False) and can_reach_revert(node): 
            argument = node.expression
        
        if argument:
            vars = get_vars_used(argument, enum_names)
            # returns True if all variables are either state variables or on the whitelist
            # AND at least one of them is nonconstant
            return all(var in str_vars or var in SOLIDITY_VARIABLE_WHITELIST for var in vars) and \
                   any(var in nonconstant_vars for var in vars)

        return False

    # Checks if the function has a stateful check, returns the result of that check
    def is_stateful_function(self, func, state_vars, enum_names) :
        # print("function %s: parameters: %s" % (func.name, list(map(str, func.parameters))))
        # print("function %s: modifiers: %s" % (func.name, [m.parameters for m in func.modifiers]))
        nodes = list(map(lambda n : self.is_stateful_node(n, state_vars, enum_names), func.nodes))
        return True in nodes

    # Checks if the contract has a function or modifier that makes a stateful check.
    def is_stateful_contract(self, contract) :
        enum_names = [e.name for e in contract.enums]
        state_vars = [sv for sv in contract.state_variables]
        # print("State vars: %s" % [str(s) for s in state_vars])
        functions = list(map(lambda f : self.is_stateful_function(f, state_vars, enum_names), contract.modifiers + contract.functions))
        return True in functions

    # Function called by Slither framework. Checks all contracts in scope of 
    # the detector and gives the result of the check on those contracts.
    def _detect(self):
        stateful_contracts = []
        for c in self.contracts :
            if self.is_stateful_contract(c) or self.inherited_state(c, stateful_contracts):
                stateful_contracts.append(c.name)

        stateful_contracts = [self.STATEFUL_MESSAGE % sc for sc in stateful_contracts]
        if stateful_contracts :
            return [self.generate_result(stateful_contracts)]
        return []
