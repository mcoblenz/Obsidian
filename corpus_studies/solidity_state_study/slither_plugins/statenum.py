from slither.detectors.abstract_detector \
    import AbstractDetector, DetectorClassification
from slither.core.expressions import *
from functools import reduce
from slither.core.declarations.solidity_variables \
    import SOLIDITY_VARIABLES,SOLIDITY_VARIABLES_COMPOSED
from slither.detectors.state.hasstate \
    import get_vars_used, SOLIDITY_VARIABLE_WHITELIST

# The following functions are convienience functions for tuples.
def fst(tup) :
    return tup[0]

def snd(tup) : 
    return tup[1]


# This class is a detector plugin for Slither. It counts the number
# of states in each contract given to it, and if that number is more than 
class StateNum(AbstractDetector):
    """
    Detect function named backdoor
    """


    ARGUMENT = 'statenum'  
    HELP = 'Gives the number of states in a given contract'
    IMPACT = DetectorClassification.HIGH
    CONFIDENCE = DetectorClassification.HIGH


    WIKI = 'STATE COUNT'
    WIKI_TITLE = 'Number of states'
    WIKI_DESCRIPTION = 'I been counting states'
    WIKI_EXPLOIT_SCENARIO = '..'
    WIKI_RECOMMENDATION = '..'
    
    # Takes in two lists and returns one list with the elements of both input
    # lists, removing duplicates in the process.
    def list_union(self, list1, list2) :
        return list(set(list1 + list2))

    # Takes in a list of indeces and a list of upper boundaries on those 
    # indeces and increments indeces in the index list until an index does
    # not reach its limit. After that happens, the rest of the input list is
    # returned unchanged and the indeces that went over their limit are set
    # to 0. If all elements go over their limits, None is returned instead.
    def increment_list(self, index_list, length_list) :
        new_indeces = []
        carry = 1
        for i in range(0,len(index_list)) :
            new_index = index_list[i] + carry
            if new_index == length_list[i] :
                new_indeces.append(0)
            else :
                new_indeces.append(new_index)
                carry = 0
        return None if carry else new_indeces
    

    # Compares two binary operation types to check if the first operation 
    # is subsumed by the second one. That is, if the first operation is used,
    # whould it also be true if the second operation were used in place of the
    # first? The truth value of this check is returned.
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
    
    # Checking for < and > is more specific than checking for !=. If 
    # all of these operations exist in the operators list, the latter
    # is removed and the input list is returned.
    def remove_redundant_ne(self, operators) :
        if BinaryOperationType.NOT_EQUAL in operators and (BinaryOperationType.LESS in operators or BinaryOperationType.GREATER in operators) :
            return list(filter(lambda x : x != BinaryOperationType.NOT_EQUAL, operators))
        return operators

    # Given a tuple containing a list of operators used and a list of 
    # right hand sides compared to, creates a list of assignments which
    # tells, for each assignment, for each rhs, the value of the variable 
    # relative to that rhs. For instance:
    #
    # value = (["!=", "=="], ["0", "_reserved"])
    #
    # returns:
    # [ {"0" : "!=", "_reserved" : "!="},
    #   {"0" : "==", "_reserved" : "!="},
    #   {"0" : "!=", "_reserved" : "=="},
    #   {"0" : "==", "_reserved" : "=="}
    # ]
    def make_assignment_list(self, value) :
        if isinstance(value, tuple) :
            assignment_list = []
            operators, rhss = value
            index_list = [0 for _ in rhss]
            length_list = [len(operators) for _ in rhss]   

            while index_list != None :
                assignment = {rhs:operators[index_list[i]] for i,rhs in enumerate(rhss)}

                assignment_list.append(assignment)
                index_list = self.increment_list(index_list, length_list)
            return assignment_list

        return value

    # Creates a dictionary of assignments for each variable, 
    # returns the dictionary
    def make_assignments(self, all_vars) :
        return {k:self.make_assignment_list(all_vars[k]) for k in all_vars.keys()}

    # Takes two dictionaries of variables mapped to the values they 
    # are compared to in state checks, and merges the dictionaries into one
    # to include the variables from both inputs.
    def merge_vars(self, var_dict_1, var_dict_2) :
        res = {}
        for var in var_dict_1 :
            if var in var_dict_2 :                
                if isinstance(var_dict_1[var], tuple) and isinstance(var_dict_2[var], tuple) :
                    ops_1, rhs_1 = var_dict_1[var]
                    ops_2, rhs_2 = var_dict_2[var]

                    res[var] = (self.remove_redundant_ne(self.list_union(ops_1, ops_2)) ,self.list_union(rhs_1,rhs_2))
                else :
                    res[var] = var_dict_1[var]
            else :
                res[var] = var_dict_1[var]
        for var in {var for var in var_dict_2 if not var in res} :
            res[var] = var_dict_2[var]
        return res

    # Traverses a state check AST in order to find all boolean checks made,
    # storing the variables on the left hand side of the check with all 
    # possible values it can hold in relation to the right hand side of the 
    # check. Returns the dictionary containing this information.
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
            right_exp = str(state_check.expression_right)
            if sc_type == BinaryOperationType.LESS or sc_type == BinaryOperationType.GREATER or sc_type == BinaryOperationType.LESS_EQUAL or sc_type == BinaryOperationType.GREATER_EQUAL :
                vars[str(state_check.expression_left)] = ([BinaryOperationType.EQUAL, BinaryOperationType.LESS, BinaryOperationType.GREATER], [right_exp])
                return vars
            if sc_type == BinaryOperationType.EQUAL or sc_type == BinaryOperationType.NOT_EQUAL:
                vars[str(state_check.expression_left)] = ([BinaryOperationType.EQUAL, BinaryOperationType.NOT_EQUAL], [right_exp])
                return vars
            if sc_type == BinaryOperationType.ANDAND or sc_type == BinaryOperationType.OROR:
                leftvars = self.derive_vars(state_check.expression_left)
                rightvars = self.derive_vars(state_check.expression_right)
                return self.merge_vars(leftvars, rightvars)
            else :
                print("Unexpected AST structure")
                return {}
        elif isinstance(state_check, Literal) :
            return vars
        elif isinstance(state_check, MemberAccess) :
            vars[str(state_check)] = [True, False]
            return vars
        elif isinstance(state_check, IndexAccess) :
            vars[str(state_check)] = [True, False]
            return vars
        elif isinstance(state_check, TupleExpression) :
            return self.derive_vars(state_check.expressions[0])
        else :
            print("Unexpected expression in check AST")
            return vars
    
    # Takes a dictionary of state checks, a single state check to satisfy, and
    # a dictionary of values assigned to each variable in every state check, 
    # and determines whether or not the given variable assignments satisfy the
    # current state check.
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
                rhs = str(state_check.expression_right)
                operation = var_vals[str(state_check.expression_left)].get(rhs)
                if not operation :
                    print("Right hand side of expression not found when searching for possible variable values.")
                    return False
                return self.is_subset_comparison(operation, sc_type)
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
        elif isinstance(state_check, TupleExpression) :
            return self.satisfies_check(state_checks, state_check.expressions[0], var_vals)
        else :
            print("unexpected")
            return False

    # Determines whether or not all of the state checks in state_check_list 
    # are satisfied by the given variable assignment, var_vals.
    def satisfies_check_list(self, state_checks, state_check_list, var_vals) :
        return reduce(
            lambda x,state_check : self.satisfies_check(state_checks, 
                                                        state_check, 
                                                        var_vals) 
                                        if x else x,
            state_check_list, 
            True)

    # Takes in a dictionary of function names to state checks in those 
    # functions, determines the possible variable assignments, then counts
    # the number of distinct function state checks satisfied by each 
    # possible assignment of variables.
    def find_states(self, state_checks) :
        states = []
        all_vars = {}

        for key in state_checks.keys() :
            for check in state_checks[key] :
                new_vars = self.derive_vars(check)
                all_vars = self.merge_vars(all_vars, new_vars)

        if not all_vars :
            return []

        all_vars = self.make_assignments(all_vars)
            
        index_list = [(0,var) for var in all_vars.keys()]
        length_list = [len(all_vars[var]) for var in all_vars.keys()]

        while (index_list != None) :
            var_vals = {k:v for k,v in map(lambda x : (snd(x), all_vars[snd(x)][fst(x)]), index_list)}
            check_assignments = list(map(lambda x : 1 if self.satisfies_check_list(state_checks, state_checks[x], var_vals) else 0, state_checks))
            encoded_truth_value = reduce(lambda x,y : 2*x + y, check_assignments, 0)

            if not encoded_truth_value in states :
                #print(var_vals)
                states.append(encoded_truth_value)

            inc_list = self.increment_list(list(map(lambda tup : fst(tup), index_list)), length_list)
            index_list = list(map(lambda tup : (inc_list[fst(tup)], snd(snd(tup))), list(enumerate(index_list)))) if inc_list else None
        return states

    # Checks whether or not a given check is stateful using the heuristic 
    # described in 
    # http://www.cs.cmu.edu/~aldrich/papers/aldrich-empirical-ecoop11.pdf
    # with the modification that any check that uses state variables and no local
    # or solidity variables is considered stateful.
    def is_stateful_check(self, check, state_vars) :
        str_vars = [str(sv) for sv in state_vars]
        constant_vars = [str(sv) for sv in state_vars if sv.is_constant]
        nonconstant_vars = [str(sv) for sv in state_vars if not sv.is_constant]
        vars = get_vars_used(check)
        uses_state = False
        for var in vars :
            if not (var in str_vars or var in SOLIDITY_VARIABLE_WHITELIST) :
                return False
            elif var in nonconstant_vars :
                uses_state = True
        if uses_state :
            print(str(check))
        return uses_state

    # Finds all stateful checks in a given function by searching that function
    # for all require, assert, and if statements. For each argument of those
    # statements, is_stateful_check is called on those checks, and if the 
    # function returns true then the check is considered a state check and
    # returned in the final list.
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
                    argument = n.expression
                
                if not argument is None :

                    if self.is_stateful_check(argument, [sv for sv in function.state_variables_read]) :
                        state_checks.append(argument)
        return state_checks

    # Counts the number of states in a given contract using the 
    # helper functions above.
    def count_states(self, contract) :
        state_checks = {}
        for f in contract.functions :
            state_checks[f.name] = self.derive_state_checks(f)

        state_checks = {k:v for (k,v) in state_checks.items() if v}
        return len(self.find_states(state_checks))

    # Implemented for use as a detector plugin in slither, this function is 
    # called in that context after the class is created with the relevant
    # variables defined, namely self.contract.
    def _detect(self):
        stateful_contracts = []
        for c in self.contracts :
            num_states = self.count_states(c)
            if num_states > 0 :
                stateful_contracts.append("Contract " + c.name + " has " + str(num_states) + " states.\n")

        if stateful_contracts :
            return [self.generate_result(stateful_contracts)]
        return []