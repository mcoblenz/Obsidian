from slither.detectors.abstract_detector \
    import AbstractDetector, DetectorClassification
from slither.core.expressions import *
from functools import reduce
from slither.core.declarations.solidity_variables \
    import SOLIDITY_VARIABLES,SOLIDITY_VARIABLES_COMPOSED
from slither.detectors.state.hasstate \
    import get_vars_used, SOLIDITY_VARIABLE_WHITELIST
from slither.detectors.state.symbolic_expression import *
from slither.slithir.operations import *
from slither.core.cfg.node import *
from slither.core.declarations.function import FunctionType

TAB = "\t"

class StateVariableType() :
    CONSTANT = 0
    MAPPING = 1
    NONCONSTANT = 2

class StateNum(AbstractDetector):
    """
    Detect function named backdoor
    """
    

    ARGUMENT = 'statenum2'  
    HELP = 'Gives the number of states in a given contract'
    IMPACT = DetectorClassification.HIGH
    CONFIDENCE = DetectorClassification.HIGH


    WIKI = 'STATE COUNT'
    WIKI_TITLE = 'Number of states'
    WIKI_DESCRIPTION = 'I been counting states'
    WIKI_EXPLOIT_SCENARIO = '..'
    WIKI_RECOMMENDATION = '..'

    def negate_binary_op(self, op) : 
        if op == BinaryOperationType.LESS :
            return BinaryOperationType.GREATER_EQUAL
        if op == BinaryOperationType.GREATER :
            return BinaryOperationType.LESS_EQUAL
        if op == BinaryOperationType.LESS_EQUAL :
            return BinaryOperationType.GREATER
        if op == BinaryOperationType.GREATER_EQUAL :
            return BinaryOperationType.LESS
        if op == BinaryOperationType.EQUAL :
            return BinaryOperationType.NOT_EQUAL
        if op == BinaryOperationType.NOT_EQUAL: 
            return BinaryOperationType.EQUAL
        if op == BinaryOperationType.ANDAND :
            return BinaryOperationType.OROR
        if op == BinaryOperationType.OROR :
            return BinaryOperationType.ANDAND
        else :
            print(str(op) + " is not a boolean operator.")
            return op

    def binary_op_conversion(self, op) :
        if op == BinaryOperationType.LESS :
            return BinaryOperator.LESS
        if op == BinaryOperationType.GREATER :
            return BinaryOperator.GREATER
        if op == BinaryOperationType.LESS_EQUAL :
            return BinaryOperator.LESS_EQUAL
        if op == BinaryOperationType.GREATER_EQUAL :
            return BinaryOperator.GREATER_EQUAL
        if op == BinaryOperationType.EQUAL :
            return BinaryOperator.EQ
        if op == BinaryOperationType.NOT_EQUAL: 
            return BinaryOperator.NEQ
        if op == BinaryOperationType.ANDAND :
            return BinaryOperator.AND
        if op == BinaryOperationType.OROR :
            return BinaryOperator.OR
        if op == BinaryOperationType.ADDITION :
            return BinaryOperator.PLUS
        if op == BinaryOperationType.SUBTRACTION :
            return BinaryOperator.MINUS
        if op == BinaryOperationType.MULTIPLICATION :
            return BinaryOperator.TIMES
        if op == BinaryOperationType.DIVISION :
            return BinaryOperator.DIV
        else :
            print(str(op) + " is not a boolean operator.")
            return op

    def solve(self, clauses) : 
        return None

    def get_conjunctions(self, check) :
        conjunctions = []
        if isinstance(check, SymbolicVariable) :
            pass
        elif isinstance(check, UnaryExpression) :
            pass
        elif isinstance(check, BinaryExpression) :
            pass
        elif isinstance(check, Literal) :
            pass
        else :
            print("Check " + str(check) + " has the wrong type.")
            return conjunctions

    def cnf(self, state_checks) :
        cnf = []
        for check in state_checks :
            cnf += [self.get_conjunctions(check)]
        return cnf

    def gen_all_combinations(self, state_checks) :
        if not state_checks :
            return []
        sc, scxs = state_checks[0], state_checks[1:]
        notsc = UnaryExpression(sc, UnaryOperator.NOT)

        res = self.gen_all_combinations(scxs)
        return [checks + [sc] for checks in res] + [checks + [notsc] for checks in res]

    def get_lvalue_var(self, exp) :
        if isinstance(exp, Identifier) :
            return exp
        elif isinstance(exp, IndexAccess) :
            return self.get_lvalue_var(exp.expression_left)
        elif isinstance(exp, MemberAccess) :
            return self.get_lvalue_var(exp.expression)
        else :
            print("Unexpected lvalue expression " + str(exp) + " of type " + str(type(exp)))
            return None

    def lvalue_contains_var(self, exp, var) :
        lvar = self.get_lvalue_var(exp)
        if lvar is None :
            return False
        return str(lvar) == str(var)

    def insert_symbols(self, exp, sigma, symvar_generator) :
        symexp = None
        if isinstance(exp, BinaryOperation) :
            sigma1, lexp = self.insert_symbols(exp.expression_left, sigma, symvar_generator)
            sigma2, rexp = self.insert_symbols(exp.expression_right, sigma1, symvar_generator)
            return sigma2, BinaryExpression(self.binary_op_conversion(exp.type), lexp, rexp)
        elif isinstance(exp, UnaryOperation) :
            sigma1, exp = self.insert_symbols(exp.expression_left, sigma, symvar_generator)
            return sigma1, UnaryExpression
        elif isinstance(exp, Identifier) :
            pass
        elif isinstance(exp, Literal) :
            pass
        elif isinstance(exp, MemberAccess) :
            pass
        elif isinstance(exp, IndexAccess) :
            pass
        elif isinstance(exp, CallExpression) :
            
            pass
        else :
            return None, sigma
        return symexp, sigma

    def derive_function_checks(self, function, init_sigma, state_vars, symvar_generator, explored) :
        sigma = init_sigma
        pc = []

        for node in function.nodes :
            if node.type == NodeType.IF :
                exp = node.expression
                symexp, sigma = self.insert_symbols(exp, sigma, symvar_generator)
                if not symexp is None :
                    pc += [symexp]
            elif node.type == NodeType.EXPRESSION :
                exp = node.expression
                if isinstance(exp, AssignmentOperation) :
                    lexp, rexp = exp.expression_left, exp.expression_right
                    lvar = self.get_lvalue_var(lexp)
                    symrexp, sigma = self.insert_symbols(rexp, sigma, symvar_generator)
                    if not symrexp is None :
                        sigma[lvar] = symrexp
                elif isinstance(exp, CallExpression) :
                    if node.contains_requires_or_assert() :
                        exp = exp.arguments[0]
                        symexp, sigma = self.insert_symbols(exp, sigma, symvar_generator)
                        if not symexp is None :
                            pc += [symexp]
                    else :
                        called = exp.called
                        if called in self.current_contract.functions and not called in explored :
                            sigma, checks = self.derive_function_checks(called, sigma, state_vars, symvar_generator, explored + [function])
                            pc += checks
            else :
                pass
        return sigma, pc
        
    def derive_state_var_type(self, state_var, functions) :
        for func in functions :
            for node in func.nodes :
                if node.type == NodeType.EXPRESSION :
                    exp = node.expression
                    if isinstance(exp, AssignmentOperation) :
                        if self.lvalue_contains_var(exp.expression_left, state_var) :
                            if isinstance(exp.expression_left, Identifier) :
                                return StateVariableType.NONCONSTANT
                            else :
                                return StateVariableType.MAPPING
        return StateVariableType.CONSTANT

    def derive_states(self, contract) :
        functions = [f for f in contract.functions_and_modifiers if not f.is_constructor]
        all_nodes = {str(f) : f.nodes for f in functions}

        state_var_type = {sv : self.derive_state_var_type(sv, functions) for sv in contract.state_variables}
        
        symvar_generator = SymbolicVariableGenerator()
        init_sigma  = {sv:symvar_generator.new_var() for sv in state_var_type.keys() if state_var_type[sv] == StateVariableType.NONCONSTANT}

        state_checks = [self.derive_function_checks(function, init_sigma, state_var_type, symvar_generator) for f in functions]
        state_checks = [pc for _, pc in state_checks]

        all_combinations = [self.cnf(pc) for pc in self.gen_all_combinations(state_checks)]
        
        states = list(filter(lambda x : not x is None, [self.solve(pc) for pc in all_combinations]))
        
        return states

    def _detect(self) :
        state_info = []

        for contract in self.contracts :
            self.current_contract = contract
            states = self.derive_states(contract)
            state_num = len(states)
            if state_num > 0 :
                contract_info = ["Contract " + contract.name + " has " + str(state_num) + " states:\n"]
                for i in range(0, len(states)) :
                    state = states[i]
                    contract_info += ["\tState " + i + " is represented by the following variable assignments:\n"]
                    for var in state.keys() :
                        contract_info += ["\t\t" + var + " = " + state[var] + "\n"]
                state_info += [self.generate_result(contract_info)]

        return state_info




