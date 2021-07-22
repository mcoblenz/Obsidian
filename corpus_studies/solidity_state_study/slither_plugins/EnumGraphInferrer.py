from slither.core.expressions import *
from slither.core.cfg.node import *
from slither.core.declarations import *
from slither.detectors.functions.modifier import is_revert
from .Graph import Edge, TransitionGraph
from .GraphInferrer import GraphInferrer, expr_is_var

# Checks if the expression is an enum constant of the type enum_type.
def expr_is_enum_val(exp: Expression, enum_type: EnumContract) -> bool:
    return isinstance(exp, MemberAccess) and isinstance(exp.expression, Identifier) and \
        exp.expression.value == enum_type

class EnumGraphInferrer(GraphInferrer):
    def __init__(self, 
                contract: Contract,
                state_var: StateVariable, 
                enum_type: EnumContract):
        super().__init__(contract, state_var)
        self.enum_type = enum_type

    @property
    def all_states(self) -> Set[str]:
        return set(self.enum_type.values)

    @property
    def uninitialized_state(self) -> Set[str]:
        return self.enum_type.values[0]
        
    # Given a node and a set of possible states, return a refined set of possible initial states
    # If the node is a require/assert, return the set of states that can make the condition true
    # If the node is a modifier call, return the preconditions for the function/modifier.
    def mergeInfo(self, 
                  node: Node, 
                  starting_states: Set[str],    
                  ending_states: Set[Optional[str]], 
                  subs: Dict[Variable, Expression], 
                  level: int) -> Set[str]:
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
            subs: Dict[LocalVariable, Expression] = dict(zip(params, arguments))
            return starting_states & self.inferEdgesFromNode(func.entry_point, starting_states, ending_states, subs, level+1)
        else:
            return starting_states.copy()

    # Look for expressions assigning a constant enum value to the state var.
    # If the RHS of an assignment is not recognized to be a constant value, assume the variable could hold any value.
    def mergeEndingStates(self, 
                          node: Node, 
                          ending_states: Set[Optional[str]]) -> Set[Optional[str]]:
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