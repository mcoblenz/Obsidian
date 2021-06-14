from slither.detectors.abstract_detector import AbstractDetector, DetectorClassification
from slither.core.expressions import *
from slither.core.cfg.node import *
from slither.core.declarations.solidity_variables import SOLIDITY_VARIABLES,SOLIDITY_VARIABLES_COMPOSED
from slither.detectors.functions.modifier import is_revert
from slither.analyses.data_dependency.data_dependency import *
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
        return {exp}
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
        return {exp}
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

# This class determines whether a contract is using states, and if so, will
# provide additional information about the states.
# The function used by the hasstate detector is is_stateful_contract.
class ContractStateDetector: 
    def __init__(self, contract):
        self.contract = contract
        self.enum_names = [e.name for e in contract.enums]
        self.state_vars = [sv for sv in contract.state_variables]

        self.str_vars = [str(sv) for sv in self.state_vars]
        self.constant_vars = [str(sv) for sv in self.state_vars if sv.is_constant]
        self.nonconstant_vars = [str(sv) for sv in self.state_vars if not sv.is_constant]

    # Determines if an expression is constant, 
    # meaning the only variables used are constant variables.
    def is_constant_node(self, node):
        vars = get_vars_used(node, self.enum_names)
        # return all(v.value.is_constant for v in vars)
        for v in vars:
            try:
                if not v.value.is_constant:
                    return False
            except AttributeError:
                return False
        return True

    # Checks if the node makes a stateful check, and gives the result of
    # that check
    def is_stateful_node(self, node, whitelist_parameters=set()) :
        # print("Node: %s" % node)
        argument = None
        if node.contains_require_or_assert() :
            #require and assert both only have one argument.
            argument = node.expression.arguments[0]
        # If the node is an if expression, we check that either branch leads to a throw/revert.
        elif node.contains_if(include_loop=False) and can_reach_revert(node): 
            argument = node.expression

        # Moved modifier check to is_stateful_function
        # elif len(node.internal_calls) > 0 and node.internal_calls[0].canonical_name in self.modifier_names:
        #     modifier = node.internal_calls[0]
        #     params = list(map(str,modifier.parameters))
        #     args = node.internal_calls_as_expressions[0].arguments
        #     assert(len(params) == len(args))
        #     whitelist = set()
        #     for (arg,param) in zip(args,params):
        #         if self.is_constant_node(arg):
        #             whitelist.add(param)
        #     if self.is_stateful_modifier(modifier,whitelist):
        #         return True
        
        if argument:
            vars = set(map(str,get_vars_used(argument, self.enum_names))) - whitelist_parameters
            # print("Vars: %s %s" % (vars, get_dependencies(vars,self.contract)))
            # a = self.contract.get_state_variable_from_name('a')
            # print("%s %s" % (a, [x.name for x in get_dependencies(a,func)]))
            # for var in vars:
            #     v = self.contract.get_state_variable_from_name(var)
            #     print("%s %s" % (v, [x.name for x in get_dependencies(v,func)]))
                # print("%s %s" % (var, is_tainted(var, func)))
            # returns True if all variables are either state variables or on the whitelist
            # AND at least one of them is nonconstant
            return all(var in self.str_vars or var in SOLIDITY_VARIABLE_WHITELIST for var in vars) and \
                   any(var in self.nonconstant_vars for var in vars)

        return False

    # Checks if the modifier has a stateful check, knowing that the parameters in
    # whitelist_parameters are instantiated with constant values
    def is_stateful_modifier(self, func, whitelist_parameters):
        nodes = list(map(lambda n : self.is_stateful_node(n,whitelist_parameters), func.nodes))
        return any(nodes)

    # Checks if the function has a stateful check, returns the result of that check
    def is_stateful_function(self, func) :
        ret = False
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
            params = list(map(str,m.parameters))
            args = next(c.arguments for c in func.calls_as_expressions if str(c.called) == m.name)
            assert(args != None)
            assert(len(args) == len(params))

            whitelist = set()
            for (arg,param) in zip(args,params):
                if self.is_constant_node(arg):
                    whitelist.add(param)
            if self.is_stateful_modifier(m,whitelist):
                ret = True

        nodes = list(map(lambda n : self.is_stateful_node(n), func.nodes))
        ret |= any(nodes)
        return ret

    # Checks if the contract has a function or modifier that makes a stateful check.
    def is_stateful_contract(self):
        functions = list(map(lambda f : self.is_stateful_function(f), self.contract.functions))
        return any(functions)

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

    WIKI_TITLE = 'Detects contracts possibly using state'
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
    
    # Function called by Slither framework. Checks all contracts in scope of 
    # the detector and gives the result of the check on those contracts.
    def _detect(self):
        stateful_contracts = []
        for c in self.contracts :
            if ContractStateDetector(c).is_stateful_contract() or self.inherited_state(c, stateful_contracts):
                stateful_contracts.append(c.name)

        stateful_contracts = [self.STATEFUL_MESSAGE % sc for sc in stateful_contracts]
        if stateful_contracts :
            return [self.generate_result(stateful_contracts)]
        return []
