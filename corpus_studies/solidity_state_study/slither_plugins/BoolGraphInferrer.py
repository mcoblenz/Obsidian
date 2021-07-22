from slither.core.expressions import *
from slither.core.cfg.node import *
from slither.core.declarations import *
from slither.detectors.functions.modifier import is_revert
from .Graph import Edge, TransitionGraph
from .GraphInferrer import GraphInferrer

# Checks if the expression is either true or false.
def expr_is_bool_val(exp: Expression) -> bool:
    return isinstance(exp, Literal) and exp.value in ['true', 'false']

# Checks if the expression is equal to the variable var.
def expr_is_var(exp: Expression, var: Variable) -> bool:
    return isinstance(exp, Identifier) and exp.value == var

class BoolGraphInferrer(GraphInferrer):
    def __init__(self, 
                contract: Contract,
                state_var: StateVariable):
        super().__init__(contract, state_var)

    @property
    def all_states(self) -> Set[str]:
        return {'true', 'false'}

    @property
    def uninitialized_state(self) -> str:
        return 'false'

    # Look for expressions assigning a constant boolean value to the state var.
    # If the RHS of an assignment is not recognized to be a constant value, assume the variable could hold any value.
    def mergeEndingStates(self, 
                          node: Node, 
                          ending_states: Set[Optional[str]]) -> Set[Optional[str]]:
        if isinstance(node.expression, AssignmentOperation) and \
           node.expression.type == AssignmentOperationType.ASSIGN and \
           expr_is_var(node.expression.expression_left, self.state_var):
            rhs = node.expression.expression_right
            if expr_is_bool_val(rhs):
                assert(isinstance(rhs, Literal))
                return {rhs.value}
            else:
                return self.all_states
        else:
            return ending_states.copy()

    # Given an expression (and an optional list of argument values for modifiers),
    # return a set of state values that will make the expression true, or None if
    # the expression is not relevant.
    # In particular, we look for expressions comparing the state var to a constant 
    # boolean value, or any boolean combination of such expressions.
    def _possibleStates(self, exp: Expression, subs: Dict[Variable, Expression]) -> Optional[Set[str]]:
        if subs is None: subs = {}

        if expr_is_var(exp, self.state_var):
            return {'true'}
        elif isinstance(exp, TupleExpression):
            return self._possibleStates(exp.expressions[0], subs)
        elif isinstance(exp, UnaryOperation) and exp.type == UnaryOperationType.BANG: # !; Take complement
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
                    # Check if exp_right is an bool value.
                    if expr_is_bool_val(exp_right):
                        if exp.type == BinaryOperationType.EQUAL:
                            return {exp_right.value}
                        elif exp.type == BinaryOperationType.NOT_EQUAL:
                            return self.all_states - {exp_right.value}
        return None