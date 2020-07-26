
from enum import Enum

class UnaryOperator(Enum) :
    NOT = 1
    NEGATION = 2

class BinaryOperator(Enum) :
    # Numeric Operators
    PLUS = 1
    MINUS = 2
    TIMES = 3
    DIV = 4
    LESS = 5
    LESS_EQUAL = 6
    GREATER = 7
    GREATER_EQUAL = 8

    #Boolean Operators
    AND = 9
    OR = 10

    #Any operand Operators
    EQ = 11
    NEQ = 12


class SymbolicExpression() :
    pass

class Literal(SymbolicExpression) :
    def __init__(self, value):
        super().__init__()
        self._value = value
    
    @property
    def value(self) :
        return self._value

'''
'''
class SymbolicVariable(SymbolicExpression) :

    def __init__(self, name, id):
        self._name = name
        self._id = id
    
    @property
    def name(self) :
        return self._name
    
    @property
    def id(self) :
        return self._id
    
    def __eq__(self, value):
        return isinstance(value, SymbolicVariable) and self.name == value.name and self.id == value.id
    
    def __ne__(self, value):
        return not self == value

class UnaryExpression(SymbolicExpression) :

    def __init__(self, operator, operand):
        super().__init__()
        assert(isinstance(operator, UnaryOperator))
        assert(isinstance(operand, SymbolicExpression))
        self._operator = operator
        self._operand = operand
    
    @property
    def operator(self) :
        return self._operator
    
    @property
    def operand(self) :
        return self._operand

class BinaryExpression(SymbolicExpression) :

    def __init__(self, operator, left_operand, right_operand):
        super().__init__()
        assert(isinstance(operator, BinaryOperator))
        assert(isinstance(left_operand, SymbolicExpression))
        assert(isinstance(right_operand, SymbolicExpression))
        self._operator = operator
        self._left_operand = left_operand
        self._right_operand = right_operand

    @property
    def operator(self) :
        return self._operator
    
    @property
    def left_operand(self) :
        return self._left_operand

    @property
    def right_operand(self) :
        return self._right_operand

'''
'''
class SymbolicVariableGenerator() :

    def __init__(self) :
        super().__init__()
        self._var_count = 0
    
    def new_var(self, name) :
        res = SymbolicVariable(name, self._var_count)
        self._var_count += 1
        return res
