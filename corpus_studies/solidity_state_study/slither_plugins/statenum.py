from slither.detectors.abstract_detector import AbstractDetector, DetectorClassification
from slither.core.expressions import *
from functools import reduce

class StateNum(AbstractDetector):
    """
    Detect function named backdoor
    """

    ARGUMENT = 'statenum'  
    HELP = 'Gives the number of states in a given contract'
    IMPACT = DetectorClassification.HIGH
    CONFIDENCE = DetectorClassification.HIGH


    WIKI = 'TODO'
    WIKI_TITLE = 'Number of states'
    WIKI_DESCRIPTION = 'I been counting states'
    WIKI_EXPLOIT_SCENARIO = '..'
    WIKI_RECOMMENDATION = '..'

    def increment_list(self, index_list, length_list) :
        new_indeces = index_list
        carry = 1
        for i in range(0,len(new_indeces)) :
            j, var = new_indeces[i]
            new_indeces[i] = (carry + j, var)
            if carry + j == length_list[i] :
                new_indeces[i] = (0, var)
            else :
                return new_indeces
        return None

    def is_subset_comparison(self, cmp1, cmp2) :
        if cmp1 == cmp2 :
            return True
        elif cmp1 == BinaryOperationType.LESS and cmp2 == BinaryOperationType.LESS_EQUAL:
            return True
        elif cmp1 == BinaryOperationType.GREATER and cmp2 == BinaryOperationType.GREATER_EQUAL :
            return True
        elif cmp1 == BinaryOperationType.EQUAL and (cmp2 == BinaryOperationType.LESS_EQUAL or cmp2 == BinaryOperationType.GREATER_EQUAL) :
            return True
        elif (cmp1 == BinaryOperationType.LESS or cmp1 == BinaryOperationType.GREATER) and cmp2 == BinaryOperationType.NOT_EQUAL :
            return True
        else :
            return False
    
    def remove_redundant_ne(self, assignments) :
        operators = list(map(lambda x : x[0] if isinstance(x,tuple) else x, assignments))
        if BinaryOperationType.NOT_EQUAL in operators and (BinaryOperationType.LESS in operators or BinaryOperationType.GREATER in operators) :
            return list(filter(lambda x : tuple(x)[0] != BinaryOperationType.NOT_EQUAL, assignments))
        return assignments
            

    def union (self, list1, list2) :
        return list(set(sorted(list1 + list2)))


    def derive_vars(self, state_check) :
        vars = {}

        if isinstance(state_check, Identifier) :
            vars[str(state_check)] = [True, False]
            return vars
        elif isinstance(state_check, CallExpression) :
            #Since every function in the contract is checked, the variables will come out of checking that function on its own
            return vars
        elif isinstance(state_check, UnaryOperation) :
            return self.derive_vars(state_check.expression)
        elif isinstance(state_check, BinaryOperation) :
            sc_type = state_check.type
            
            if sc_type == BinaryOperationType.LESS or sc_type == BinaryOperationType.GREATER or sc_type == BinaryOperationType.LESS_EQUAL or sc_type == BinaryOperationType.GREATER_EQUAL :
                right_exp = str(state_check.expression_right)
                vars[str(state_check.expression_left)] = [(BinaryOperationType.EQUAL, right_exp), (BinaryOperationType.LESS, right_exp), (BinaryOperationType.GREATER, right_exp)]
                return
            if sc_type == BinaryOperationType.EQUAL or sc_type == BinaryOperationType.NOT_EQUAL:
                right_exp = str(state_check.expression_right)
                vars[str(state_check.expression_left)] = [(BinaryOperationType.EQUAL, right_exp), (BinaryOperationType.NOT_EQUAL, right_exp)]
                return vars
            if sc_type == BinaryOperationType.ANDAND or sc_type == BinaryOperationType.OROR:
                leftvars = self.derive_vars(state_check.expression_left)
                rightvars = self.derive_vars(state_check.expression_right)
                return {**leftvars, **rightvars}
            else :
                print("Unexpected AST structure")
                return {}
        elif isinstance(state_check, Literal) :
            return vars
        elif isinstance(state_check, MemberAccess) or isinstance(state_check, IndexAccess) :
            vars[str(state_check)] = [True, False]
            return vars
        else :
            print("unexpected expression in check AST")
            return vars

    def satisfies_check(self, state_checks, state_check, var_vals) :
        if isinstance(state_check, Identifier) :
            #Here the identifier must be a bool, we catch all other cases earlier
            return var_vals[str(state_check)]
        elif isinstance(state_check, UnaryOperation) :
            #The only unary operator possible is !, the not operator.
            return not self.satisfies_check(state_checks, state_check.expression, var_vals)
        elif isinstance(state_check, BinaryOperation) :
            sc_type = state_check.type
            if sc_type == BinaryOperationType.LESS or sc_type == BinaryOperationType.GREATER or sc_type == BinaryOperationType.LESS_EQUAL or sc_type == BinaryOperationType.GREATER_EQUAL or sc_type == BinaryOperationType.EQUAL or sc_type == BinaryOperationType.NOT_EQUAL :
                operation, rhs = var_vals[str(state_check.expression_left)]
                if rhs == str(state_check.expression_right) :
                    return self.is_subset_comparison(operation, sc_type)
                else :
                    print("Multiple rhs for same lhs found, more information needed to distinguish the two")
                    return False
            if sc_type == BinaryOperationType.ANDAND:
                return self.satisfies_check(state_checks, state_check.expression_left, var_vals) and self.satisfies_check(state_checks, state_check.expression_right, var_vals)
            if sc_type == BinaryOperationType.OROR:
                return self.satisfies_check(state_checks, state_check.expression_left, var_vals) or self.satisfies_check(state_checks, state_check.expression_right, var_vals)
            else :
                print("Unexpected operator in check AST")
                return False
        elif isinstance(state_check, Literal) :
            #This case should never be reached, but on the off chance that it is we return the literal's value
            return state_check.value
        elif isinstance(state_check, MemberAccess) or isinstance(state_check, IndexAccess) :
            return var_vals[str(state_check)]
        else :
            print("unexpected")
            return False


    def satisfies_check_list(self, state_checks, state_check_list, var_vals) :
        if not state_check_list :
            return 1
        else :
            state_check = state_check_list[0]
            state_check_xs = state_check_list[1:]

            if not self.satisfies_check(state_checks, state_check, var_vals) :
                return 0

            return self.satisfies_check_list(state_checks, state_check_xs, var_vals)

    def find_states(self, state_checks) :
        states = []
        all_vars = {}

        for key in state_checks.keys() :
            for check in state_checks[key] :
                new_vars = self.derive_vars(check)
                for new_var in new_vars.keys() :
                    if not new_var in all_vars.keys() :
                        all_vars[new_var] = new_vars[new_var]
                    else :
                        all_vars[new_var] = self.remove_redundant_ne(self.union(all_vars[new_var], new_vars[new_var]))

        if not all_vars :
            return []
            
        index_list = [(0,var) for var in all_vars.keys()]
        length_list = [len(all_vars[var]) for var in all_vars.keys()]

        while (index_list != None) :
            var_vals = {k:v for k,v in map(lambda x : (x[1], all_vars[x[1]][x[0]]), index_list)}
            check_assignments = list(map(lambda x : 1 if self.satisfies_check_list(state_checks, state_checks[x], var_vals) else 0, state_checks))
            encoded_truth_value = reduce(lambda x,y : 2*x + y, check_assignments, 0)

            if not encoded_truth_value in states :
                states.append(encoded_truth_value)

            index_list = self.increment_list(index_list, length_list)
        return states

        
    def derive_state_checks(self, function) :
        state_checks = []
        for m in function.modifiers :
            state_checks.extend(self.derive_state_checks(m))
        for n in function.nodes :
                argument = None
                if n.contains_require_or_assert() :
                    #There's only one argument for both require and assert
                    argument = n.expression.arguments[0]
                elif n.contains_if() :
                    argument = n.expression.if_expression
                
                if not argument is None :
                    if bool(n.state_variables_read) and not bool(n.local_variables_read) :
                        state_checks.append(argument)
        return state_checks


    def count_states(self, contract) :
        state_checks = {}
        for f in contract.functions :
            state_checks[f.name] = self.derive_state_checks(f)

        state_checks = {k:v for (k,v) in state_checks.items() if v}

        return self.find_states(state_checks)


    def _detect(self):
        stateful_contracts = []
        for c in self.contracts :
            num_states = len(self.count_states(c))
            if num_states > 0 :
                stateful_contracts.append("Contract " + c.name + " has " + str(num_states) + " states.\n")

        if stateful_contracts :
            return [self.generate_result(stateful_contracts)]
        return []