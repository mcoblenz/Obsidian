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
# Solidity variables that are not allowed to appear in a require condition.
SOLIDITY_VARIABLE_BLACKLIST = ["msg.sender", "tx.origin"]

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
        self.state_vars = [sv for sv in contract.state_variables]
        self.nonconstant_vars = [sv for sv in self.state_vars if not sv.is_constant]
        # Getters are functions where all return statements contain values that flow from fields
        # Returns a dictionary from function names (strings) to fields that appear in them
        self.getters = {}
        for m in self.contract.functions:
            fields = self.is_getter(m)
            if fields is not None:
                self.getters[str(m)] = fields

    # Gets all variables used in an expression, returning whether the expression
    # is a valid condition for a state check.
    # In particular, all calls in the expression must be getters defined by the contract.
    def gather_vars(self, exp: Expression, vars: Set[Variable]) -> bool:
        if isinstance(exp, Identifier):
            vars.add(exp.value)
            return True
        elif isinstance(exp, CallExpression):
            if str(exp.called) in self.getters:
                vars |= (self.getters[str(exp.called)])
                return all(self.gather_vars(arg, vars) for arg in exp.arguments)
            return False
        elif isinstance(exp, UnaryOperation):
            return self.gather_vars(exp.expression, vars)
        elif isinstance(exp, BinaryOperation):
            return self.gather_vars(exp.expression_left, vars) and self.gather_vars(exp.expression_right, vars)
        elif isinstance(exp, Literal):
            return True
        elif isinstance(exp, IndexAccess):
            return self.gather_vars(exp.expression_left, vars) and self.gather_vars(exp.expression_right, vars)
        elif isinstance(exp, MemberAccess):
            return True
        elif isinstance(exp, TupleExpression):
            # There should be exactly one element in the tuple 
            #     (that is, they should act as parenthesis).
            # Comparing tuples for equality as a Solidity language feature
            #     does not exist at the time of writing.
            return self.gather_vars(exp.expressions[0], vars)
        elif isinstance(exp, TypeConversion):
            return self.gather_vars(exp.expression, vars)
        return False

    # Determines if an expression is constant, 
    # meaning the only variables used are constant variables.
    def is_constant_expr(self, exp: Expression):
        vars = set()
        if self.gather_vars(exp, vars):
            for v in vars:
                if isinstance(v, SolidityVariableComposed):
                    return False
                elif not v.is_constant:
                    return False
            return True
        else: return False

    # Check if a function is a getter, and if so, returns a list of state variables used.
    # Otherwise returns None
    def is_getter(self, func: Function) -> Optional[Set[Variable]]:
        # Check if a node is a return statement.
        # Returns None if the return statement does not rely on state variables,
        # and returns empty set if the node is not a return statement.
        def is_getter_node(node: Node) -> Set[Variable]:
            if (node.type == NodeType.RETURN) and node.expression is not None:
                vars = set()
                # There is a potential bug here: at this point, getters is still being initialized, but 
                # gather_vars assumes getters is initialized to check if any function calls are to getters.
                # This may be resolved by iterating over the functions according to a topological ordering of function dependencies.
                # On the other hand, it's not clear how often a getter function actually calls other getter functions.
                if self.gather_vars(node.expression, vars):
                    return vars
                else: return None
            return set()
        if func.view:
            vars:Set[Variable] = set()
            for node in func.nodes:
                node_vars = is_getter_node(node)
                if node_vars is None:
                    return None
                else:
                    vars |= node_vars
            return vars
        return None

    # Checks if the node makes a stateful check
    # Returns a set of state variables used, or an empty set if none are found
    def is_stateful_node(self, node: Node, func: Function, whitelist_parameters: Set[Variable] = set()) -> Set[StateVariable]:
        # Return a set of all (nonconstant) state vars that the variable is dependent on
        def get_state_vars_used(var: Variable) -> Set[StateVariable]:
            if var in self.state_vars:
                if var in self.nonconstant_vars:
                    return {var}
                else:
                    return set()
            else:
                # If var is dependent on itself, remove it to stop an infinite loop
                next_vars = get_dependencies(var,func) - {var}
                return set().union(*map(get_state_vars_used, next_vars))
        # Check if a variable is dependent only on state variables
        def is_dependent_on_state_vars(var: Variable):
            # print("%s: %s" % (var,list(map(str,get_dependencies(var,func)))))
            if var in func.parameters or var.name in SOLIDITY_VARIABLE_BLACKLIST:
                return False
            # If var is dependent on itself, remove it to stop an infinite loop
            next_vars = get_dependencies(var,func) - {var}
            return var in self.state_vars or all(map(is_dependent_on_state_vars, next_vars))
        # Check if a variable is dependent on at least one nonconstant state variable
        def is_dependent_on_nonconstant(var: Variable):
            # If var is dependent on itself, remove it to stop an infinite loop
            next_vars = get_dependencies(var,func) - {var}   
            return var in self.nonconstant_vars or any(map(is_dependent_on_nonconstant,next_vars))
        def is_stateful_argument(argument: Expression) -> Set[Variable]:
            vars:Set[Variable] = set()
            if self.gather_vars(argument, vars):
                vars -= whitelist_parameters
                # returns set of used state variables if all variables are either dependent only on state variables or on the whitelist
                # AND at least one of them is nonconstant (or dependent on nonconstant variables)
                # print("arg: %s" % argument)
                # print("vars: %s" % [v.name for v in vars])
                if all(is_dependent_on_state_vars(var) or str(var) in SOLIDITY_VARIABLE_WHITELIST for var in vars) and \
                   any(is_dependent_on_nonconstant(var) for var in vars):
                    return set().union(*map(get_state_vars_used, vars))
                else: return set()
            else: return set()
            
        argument = None
        if node.contains_require_or_assert() :
            #require and assert both only have one argument.
            argument = node.expression.arguments[0]
        # If the node is an if expression, we check that either branch leads to a throw/revert.
        elif node.contains_if(include_loop=False) and can_reach_revert(node): 
            argument = node.expression
        
        if argument is not None:
            return is_stateful_argument(argument)
        return set()

    # Checks if the modifier has a stateful check, knowing that the parameters in
    # whitelist_parameters are instantiated with constant values
    def is_stateful_modifier(self, func: Function, whitelist_parameters: Set[Variable]) -> Set[StateVariable]:
        return set().union(*map(lambda n : self.is_stateful_node(n,func,whitelist_parameters), func.nodes))

    # Checks if the function  or any called modifiers has a stateful check
    # Returns a set of state variables used, or an empty set if none are found
    def is_stateful_function(self, func: Function) -> Set[StateVariable]:
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
            args = next(c.arguments for c in func.calls_as_expressions if isinstance(c, CallExpression) and str(c.called) == m.name)
            assert(args != None)
            assert(len(args) == len(params))

            whitelist = set()
            for (arg,param) in zip(args,params):
                if self.is_constant_expr(arg):
                    whitelist.add(param)
            ret |= self.is_stateful_modifier(m,whitelist)

        ret |= set().union(*map(lambda n : self.is_stateful_node(n,func), func.nodes))
        return ret

    # Checks if the contract has a function or modifier that makes a stateful check.
    def is_stateful_contract(self) -> bool:
        # print("GETTERS: %s" % [(k,[m.name for m in v]) for k,v in self.getters.items()])
        ret = set()
        for f in self.contract.functions:
            # TODO: Return this information for the detector
            vars = self.is_stateful_function(f)
            print("Function %s: fields: %s" % (f.name, [v.name for v in vars]))
            ret |= vars
        return len(ret) > 0

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