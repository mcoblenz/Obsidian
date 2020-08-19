from slither.detectors.abstract_detector \
    import AbstractDetector, DetectorClassification
from slither.core.expressions import *
from functools import reduce
from slither.core.declarations.solidity_variables \
    import SOLIDITY_VARIABLES,SOLIDITY_VARIABLES_COMPOSED
from slither.detectors.state.hasstate \
    import get_vars_used, SOLIDITY_VARIABLE_WHITELIST
from slither.core.cfg.node import *
from slither.core.solidity_types.elementary_type import Int, Uint, Ufixed, Fixed, Byte
from slither.core.solidity_types.user_defined_type import UserDefinedType
from slither.core.declarations.function import FunctionType
from slither.core.declarations.function import Function as Fn
from slither.core.solidity_types.array_type import ArrayType
from slither.core.declarations.solidity_variables import SOLIDITY_VARIABLES, SOLIDITY_VARIABLES_COMPOSED


INTS = Int + Uint + ['address']
BOOLS = ['bool']
STRINGS = ['string']
REALS = Fixed + Ufixed
BYTES = Byte

from z3 import *

TAB = "\t"

class StateMachine(AbstractDetector):

    ENUM_SORTS = []

    ARGUMENT = 'statemachine'  
    HELP = 'Gives the number of states in a given contract'
    IMPACT = DetectorClassification.HIGH
    CONFIDENCE = DetectorClassification.HIGH


    WIKI = 'STATE COUNT'
    WIKI_TITLE = 'Number of states'
    WIKI_DESCRIPTION = 'Counts state numbers.'
    WIKI_EXPLOIT_SCENARIO = '..'
    WIKI_RECOMMENDATION = '..'

    def valid_type(self, typ) :
        return isinstance(typ, (ElementaryType, UserDefinedType)) 

    def get_sym_info(self, var_name, return_type) :
        typ = return_type
        if var_name in SOLIDITY_VARIABLES.keys() : 
            typ = SOLIDITY_VARIABLES[var_name]
        elif var_name in SOLIDITY_VARIABLES_COMPOSED.keys() :
            typ = SOLIDITY_VARIABLES_COMPOSED[var_name]
        if typ in INTS or typ in BYTES or isinstance(typ, Contract):
            return IntSort(), Int(var_name) 
        elif typ in REALS :
            return RealSort(), Real(var_name)
        elif isinstance(typ, Structure):
            return ArraySort(IntSort(), IntSort()), Array(var_name, IntSort(), IntSort())
        elif typ in BOOLS :
            return BoolSort(), Bool(var_name)
        elif typ in STRINGS :
            return StringSort(), String(var_name)
        elif isinstance(typ, Enum) :
            if var_name in [str(s) for s in self.ENUM_SORTS] :
                sort = [s for s in self.ENUM_SORTS if var_name == str(s)][0]
                return sort, Const(var_name, sort)
            else :
                sort = DeclareSort(str(return_type))
                self.ENUM_SORTS += [sort]
                return sort, Const(var_name, sort)
        else :
            #print("Declaring new sort " + str(return_type) + " for " + var_name)
            S = DeclareSort(str(return_type))
            return S, Const(var_name, S)


    def convert_to_smt(self, exp, sigma, reachable_functions) :
        if isinstance(exp, Identifier) :
            if str(exp) in sigma.keys() :
                return sigma[str(exp)]
            else :
                _, sym = self.get_sym_info(str(exp), exp.type)
                return sym
        elif isinstance(exp, CallExpression) :
            func = exp.called
            if isinstance(func, Identifier) :
                if isinstance(func.value, (Fn, SolidityFunction)) :
                    if not func.value.return_type is None :
                        _, sym = self.get_sym_info(str(func), func.value.return_type[0].name)
                        return sym


            elif isinstance(func, MemberAccess) :
                func = func.member_name
                if isinstance(exp.called.expression, Identifier) :
                    if isinstance(exp.called.expression.value.type, ArrayType) :
                        return Int(str(exp.called.expression) + " length")
                if func in [str(f) for f in reachable_functions if (not isinstance(f, Fn)) or (not f.is_shadowed)] :
                    func = [f for f in reachable_functions if (not isinstance(f, Fn)) or (not f.is_shadowed and str(f) == func)][0]
                    if isinstance(func, Fn) :
                        if not func.return_type is None :
                            _, sym = self.get_sym_info(str(func), func.return_type[0].name)
                            return sym
                    else :
                        _, sym = self.get_sym_info(str(func), func.type.type)
                        return sym
        elif isinstance(exp, UnaryOperation) :
            smt = self.convert_to_smt(exp.expression, sigma, reachable_functions)
            if exp.type == UnaryOperationType.BANG :
                return Not(smt)
            elif exp.type == UnaryOperationType.MINUS_PRE :
                return -smt
            elif exp.type == UnaryOperationType.PLUS_PRE :
                return smt
            elif exp.type in [UnaryOperationType.PLUSPLUS_PRE, UnaryOperationType.PLUSPLUS_POST] :
                return smt + 1
            elif exp.type in [UnaryOperationType.MINUSMINUS_PRE, UnaryOperationType.MINUSMINUS_POST] :
                return smt - 1
            else :
                print("Unexpected unary operator: " + str(exp.type))
        elif isinstance(exp, BinaryOperation) :
            lsmt = self.convert_to_smt(exp.expression_left, sigma, reachable_functions)
            rsmt = self.convert_to_smt(exp.expression_right, sigma, reachable_functions)
            if exp.type == BinaryOperationType.LESS :
                return lsmt < rsmt
            elif exp.type == BinaryOperationType.GREATER :
                return lsmt > rsmt
            elif exp.type == BinaryOperationType.LESS_EQUAL :
                return lsmt <= rsmt
            elif exp.type == BinaryOperationType.GREATER_EQUAL :
                return lsmt >= rsmt
            elif exp.type == BinaryOperationType.EQUAL :
                return lsmt == rsmt
            elif exp.type == BinaryOperationType.NOT_EQUAL :
                return lsmt != rsmt
            elif exp.type == BinaryOperationType.ANDAND :
                return And(lsmt, rsmt)
            elif exp.type == BinaryOperationType.OROR :
                return Or(lsmt, rsmt)
            elif exp.type == BinaryOperationType.POWER:
                return lsmt ** rsmt
            elif exp.type == BinaryOperationType.MULTIPLICATION:
                return lsmt * rsmt
            elif exp.type == BinaryOperationType.DIVISION:
                return lsmt / rsmt
            elif exp.type == BinaryOperationType.MODULO:
                return lsmt % rsmt
            elif exp.type == BinaryOperationType.ADDITION:
                return lsmt + rsmt
            elif exp.type == BinaryOperationType.SUBTRACTION:
                return lsmt - rsmt
            elif exp.type == BinaryOperationType.LEFT_SHIFT:
                return lsmt << rsmt
            elif exp.type == BinaryOperationType.RIGHT_SHIFT:
                return lsmt >> rsmt
            elif exp.type == BinaryOperationType.AND:
                return lsmt & rsmt
            elif exp.type == BinaryOperationType.OR:
                return lsmt | rsmt
            elif exp.type == BinaryOperationType.CARET :
                return Int(str(lsmt) + " ^ " + str(rsmt))
            else :
                print("Unexpected binary operator: " + str(exp.type))
        elif isinstance(exp, Literal) :
            if exp.type.name in INTS or exp.type.name in REALS :
                return int(exp.value, 0)
            elif exp.type.name in BOOLS :
                return True if exp.value == "true" else False
            else :
                return exp.value
        elif isinstance(exp, MemberAccess) or isinstance(exp, IndexAccess) :
            _, sym = self.get_sym_info(str(exp), exp.type)
            return sym            
        elif isinstance(exp, TupleExpression) :
            if len(exp.expressions) == 1 :
                return self.convert_to_smt(exp.expressions[0], sigma, reachable_functions)
            print("Tuples of more than one element not supported currently.")
        elif isinstance(exp, TypeConversion) :
            if exp.type.type == 'address' :
                return Int(str(exp))
            return self.convert_to_smt(exp.expression, sigma, reachable_functions)
        else :
            print("Unexpected expression in check AST: " + str(exp))
        return None
    
    def derive_symbolic_value(self, state_var) :
        if isinstance(state_var.type, ElementaryType):
            _, sym = self.get_sym_info(str(state_var), state_var.type.name)
            return sym
        else : 
            #Should be a UserDefinedType
            _, sym = self.get_sym_info(str(state_var), (state_var.type.type))
            return sym

    def get_constant_vars(self, functions) :
        constructor_fns = [f for f in functions if f.is_constructor or f.is_constructor_variables]
        non_constructor_fns = [f for f in functions if f not in constructor_fns]
        constant_vars = {}
        candidates = {}
        for f in constructor_fns :
            if f._function_type in [FunctionType.CONSTRUCTOR, FunctionType.CONSTRUCTOR_VARIABLES] :
                sigma = {}
                for param in f.parameters :
                    _, sym = self.get_sym_info(str(param), param.type.name)
                    sigma[str(param)] = sym
                for node in f.nodes :
                    exp = node.expression
                    if node.expression != None :
                        if isinstance(exp, AssignmentOperation) and isinstance(exp.expression_left, Identifier) :
                            candidates[exp.expression_left] = exp.expression_right, dict(sigma)
            elif f._function_type == FunctionType.CONSTRUCTOR_CONSTANT_VARIABLES :

                for node in f.nodes :
                    exp = node.expression
                    if not exp is None : 
                        lexp, rexp = exp.expression_left, exp.expression_right
                        constant_vars[str(lexp)] = self.convert_to_smt(rexp, {}, functions)
        for f in non_constructor_fns :
            for node in f.nodes :
                if node.type == NodeType.EXPRESSION :
                    exp = node.expression
                    if isinstance(exp, AssignmentOperation) :
                        lexp = exp.expression_left
                        candidates = {l : candidates[l] for l in candidates.keys() if str(l) != str(lexp)}
        for lexp in candidates.keys() :
            constant_vars[str(lexp)] = self.convert_to_smt(candidates[lexp][0], candidates[lexp][1], functions)
        return constant_vars

    def is_state_check(self, check, state_vars, constants, reachable_functions) :
        if isinstance(check, Identifier) :
            return str(check) in state_vars or str(check) in constants
        elif isinstance(check, UnaryOperation) :
            return self.is_state_check(check.expression, state_vars, constants, reachable_functions)
        elif isinstance(check, BinaryOperation) :
            return self.is_state_check(check.expression_left, state_vars, constants, reachable_functions) and self.is_state_check(check.expression_right, state_vars, constants, reachable_functions) 
        elif isinstance(check, CallExpression) :
            return False
        elif isinstance(check, Literal) :
            return True
        elif isinstance(check, MemberAccess) :
            #For now, we say that member and index accesses are not part of the state
            return False
        elif isinstance(check, IndexAccess) :
            return False
        elif isinstance(check, TypeConversion) :
            return self.is_state_check(check.expression, state_vars, constants, reachable_functions)
        elif isinstance(check, TupleExpression) :
            if len(check.expressions) == 1 :
                return self.is_state_check(check.expressions[0], state_vars, constants, reachable_functions)
            return False
        else :
            print(TAB+"Invalid check type " + str(type(check)) + " for check " + str(check))
            return False

    def state_check_includes_var(self, state_check, state_var) :
        if isinstance(state_check, Identifier) :
            return str(state_check) == str(state_var) 
        elif isinstance(state_check, UnaryOperation) :
            return self.state_check_includes_var(state_check.expression, state_var)
        elif isinstance(state_check, BinaryOperation) :
            return self.state_check_includes_var(state_check.expression_left, state_var) or self.state_check_includes_var(state_check.expression_right, state_var) 
        elif isinstance(state_check, CallExpression) :
            return False
        elif isinstance(state_check, Literal) :
            return False
        elif isinstance(state_check, MemberAccess) :
            #For now, we say that member and index accesses are not part of the state
            return False
        elif isinstance(state_check, IndexAccess) :
            return False
        elif isinstance(state_check, TypeConversion) :
            return self.state_check_includes_var(state_check.expression, state_var)
        elif isinstance(state_check, TupleExpression) :
            if len(state_check.expressions) == 1 :
                return self.state_check_includes_var(state_check.expressions[0], state_var)
            return False
        else :
            return False

    def is_identical_check(self, check1, check2) :
        solver = Solver()
        solver.add(check1 == check2)
        return solver.check() == "sat"

    def filter_identical_checks(self, check_list) :
        if not check_list :
            return []
        check = check_list[0]
        return self.filter_identical_checks(list(filter(lambda x : not self.is_identical_check(x,check), check_list[1:])))

    def find_all_checks(self, state_var, functions, reachable_functions, constants, init_sigma) :
        checks = []
        for function in functions :
            checks += self.find_all_checks(state_var, function.modifiers, reachable_functions, constants, init_sigma)
            for node in function.nodes :
                if node.type == NodeType.EXPRESSION :
                    exp = node.expression
                    if node.contains_require_or_assert() :
                        exp = exp.arguments[0]
                        if self.is_state_check(exp, init_sigma.keys(), constants, reachable_functions) :
                            if self.state_check_includes_var(exp, state_var) :
                                checks += [node]
                    elif isinstance(exp, CallExpression) :
                        called = exp.called
                        if isinstance(called, MemberAccess) :
                            if called.member_name in [str(f) for f in reachable_functions] :
                                func = [f for f in reachable_functions if called.member_name == str(f)][0]
                                checks += self.find_all_checks(state_var, [func], reachable_functions, constants, init_sigma)
                        else :
                            if str(called) in [str(f) for f in reachable_functions] :
                                checks += self.find_all_checks(state_var, [called.value], reachable_functions, constants, init_sigma)
                elif node.type == NodeType.IF :
                    exp = node.expression
                    if self.is_state_check(exp, init_sigma.keys(), constants, reachable_functions) :
                        if self.state_check_includes_var(exp, state_var) :
                            checks += [node]
        return list(set(checks))
        
    def assign_in_sigma(self, assignment_exp, sigma, reachable_functions) :
        if isinstance(assignment_exp, AssignmentOperation) :
            var, assign = assignment_exp.expression_left, assignment_exp.expression_right
            if isinstance(var, Identifier) :
                sigma[str(var)] = self.convert_to_smt(assign, sigma, reachable_functions)
        return sigma

    def get_clause(self, nodes, if_count=0, target=None) :
        if not nodes :
            return []
        node = nodes[0]
        if node.type == NodeType.IF :
            if_count += 1
        if target is None :
            if node.type in [NodeType.ENDIF, NodeType.RETURN] :
                if not if_count :
                    return [] if node.type == NodeType.ENDIF else [node]
                if_count -= 1
        if node == target :
            return []
        return [node] + self.get_clause(nodes[1:], if_count, target)

    def get_true_clause(self, nodes, son_false) :
        return self.get_clause(nodes, target=son_false)

    def get_false_clause(self, nodes, son_false) :
        if not son_false is None :
            for i in range(len(nodes)) :
                if nodes[i] == son_false :
                    return self.get_clause(nodes[i:])
        return []

    def skip_if(self, nodes, if_count=0) :
        if not nodes :
            return []
        node = nodes[0]
        if node.type == NodeType.IF :
            if_count += 1
        elif node.type in [NodeType.RETURN, NodeType.ENDIF] :
            if not if_count :
                return nodes[1:]
            if_count -= 1
        return self.skip_if(nodes[1:], if_count)

    def find_updates_in_function(self, state_var, nodes, reachable_functions, constants, state_vars, init_sigma, init_pc) :
        if not nodes :
            return init_sigma, init_pc, []
        node = nodes[0]   
        nodexs = nodes[1:]
        sigma = init_sigma
        pc = init_pc
        updates = []

        if node.type == NodeType.VARIABLE :
            sigma = self.assign_in_sigma(node.expression, sigma, reachable_functions)

        elif node.type == NodeType.EXPRESSION :
            exp = node.expression
            if node.contains_require_or_assert() :           
                pc = And(pc,self.convert_to_smt(exp.arguments[0], sigma, reachable_functions))
            elif isinstance(exp, AssignmentOperation) :   
                if str(exp.expression_left) == str(state_var) :
                    updates += [(pc,self.convert_to_smt(exp.expression_right, sigma, reachable_functions))]
                sigma = self.assign_in_sigma(exp, sigma, reachable_functions)
            elif isinstance(exp, CallExpression) :
                called = exp.called
                follow = True
                if isinstance(called, MemberAccess) :
                    if not (isinstance(called.expression, Identifier) and isinstance(called.expression.value, Contract)) :
                        follow = False
                    called = called.member_name
                called = str(called)
                if follow and called in [str(f) for f in reachable_functions] :
                    func = [f for f in reachable_functions if called == str(f) and not f.is_shadowed][0]

                    #if not (isinstance(exp.called.expression, Identifier) and isinstance(exp.called.expression.value, Contract)) :
                        # No need to case on state variables, they should never be called alone in an expression, 
                        # and if they are it should have no effect
                    if isinstance(func, Fn) :
                        if len(func.parameters) == len(exp.arguments) :
                            for i in range(len(func.parameters)) :
                                sigma[str(func.parameters[i])] = self.convert_to_smt(exp.arguments[i], sigma, reachable_functions)
                            sigma, pc, updates = self.find_updates_in_function(state_var, func.nodes, reachable_functions, constants, state_vars, sigma, pc)

        elif node.type == NodeType.IF :
            exp = node.expression
            true_nodes, false_nodes = self.get_true_clause(nodexs, node.son_false) + self.skip_if(nodes[1:]), \
                                      self.get_false_clause(nodexs, node.son_false) + self.skip_if(nodes[1:])
            
            condition = self.convert_to_smt(exp, sigma, reachable_functions)

            _,_,updates = self.find_updates_in_function(state_var, false_nodes, reachable_functions, constants, state_vars, sigma, And(pc, Not(condition)))
            nodexs = true_nodes
            pc = And(pc, condition)

        elif node.type in [NodeType.RETURN, NodeType.THROW] :
            return sigma, pc, []

        sigma, pc, new_updates = self.find_updates_in_function(state_var, nodexs, reachable_functions, constants, state_vars, sigma, pc)
        return sigma, pc, updates + new_updates

    def find_updates(self, state_var, functions, reachable_functions, constant_vars, state_vars, init_sigma) :
        updates = []
        for func in functions :
            sigma = dict(init_sigma)
            for param in func.parameters :
                _, sym = self.get_sym_info(str(param), param.type.name)
                sigma[str(param)] = sym
            nodes = list(func.nodes)
            for mod in func.modifiers :
                for param in mod.parameters :
                    _, sym = self.get_sym_info(str(param), param.type.name)
                    sigma[str(param)] = sym
                nodes = mod.nodes + nodes
            _, _, new_updates = self.find_updates_in_function(state_var, nodes, reachable_functions, constant_vars, state_vars, sigma, True)
            updates += new_updates
        return updates
    
    def add_each_check(self, combo, checks) :
        new_combos = []
        for check in checks :
            new_combos += [combo + [check]]
        return new_combos
    
    
    
    '''
    def generate_states(self, state_var_checks) :
        all_combos = [[]]
        for sv in state_var_checks.keys() :
            new_combos = []
            for combo in all_combos : 
                new_combos += self.add_each_check(combo, state_var_checks[sv])
            all_combos = new_combos
        all_combos = [And(combo) for combo in all_combos]
        valid_states = {}
        for combo in all_combos : 
            solver = Solver()
            if solver.check(combo) == "sat" :
                unique = True
                for i in valid_states.keys() :
                    state = valid_states[i]
                    if solver.check(combo == state) == "sat" :
                        unique = False
                        break
                if unique :
                    valid_states[len(valid_states.keys())] = combo
        return valid_states

    def generate_transitions(self, states, updates, sigma) :
        transitions = {i : [] for i in states.keys()}
        for var in updates.keys() :
            for guard, assignment in updates[var] :
                possible_states = []
                solver = Solver()
                for i in states.keys() :
                    state = states[i]
                    if solver.check(state == guard) == "sat" :
                        possible_states += [i]
                condition = sigma[str(var)] == assignment
                
                


        return transitions
    '''

    def derive_states(self, contract) :
        print(str(contract))

        public_functions = [f for f in contract.functions_entry_points if not (f.is_constructor or f.is_constructor_variables)]
        reachable_functions = [f for f in contract.all_functions_called if not (f.is_constructor or f.is_constructor_variables)]
        reachable_functions += [f for _,f in contract.all_library_calls if not (f.is_constructor or f.is_constructor_variables)]
        reachable_functions += [f for _,f in contract.all_high_level_calls if (isinstance(f, Fn) and not (f.is_constructor or f.is_constructor_variables))]
        reachable_functions += [var for _,var in contract.all_high_level_calls if isinstance(var, StateVariable)]

        
        constant_vars = self.get_constant_vars(contract.all_functions_called)
        print(TAB+str(constant_vars))

        state_vars = [sv for sv in contract.state_variables if str(sv) not in constant_vars.keys() and self.valid_type(sv.type)]

        state_var_type = {str(sv) : self.derive_symbolic_value(sv) for sv in state_vars}
        init_sigma = {sv : state_var_type[sv] for sv in state_var_type.keys() if not state_var_type[sv] is None}        

        state_var_checks = {sv : self.find_all_checks(sv, public_functions, reachable_functions, constant_vars, init_sigma) for sv in init_sigma.keys()}

        relevant_state_vars = [sv for sv in state_var_checks.keys() if state_var_checks[sv]]

        updates = {sv : self.find_updates(sv, public_functions, reachable_functions, constant_vars, state_vars, init_sigma) for sv in relevant_state_vars}

        #states = self.generate_states(state_var_checks)
        print(TAB+str(state_var_checks))
        print(TAB+str(updates))

        '''
        for sv in updates.keys() :
            print(TAB + "State var " + str(sv) + " has updates:")
            for check in updates[sv] :
                print(TAB+TAB+str(check))
        '''
        return []

    def _detect(self) :
        state_info = []

        for contract in self.contracts :
            self.current_contract = contract
            self.library = contract.all_library_calls
            states = self.derive_states(contract)
            state_num = 0#len(states)
            if state_num > 0 :
                contract_info = ["Contract " + contract.name + " has " + str(state_num) + " states:\n"]
                for i in range(0, len(states)) :
                    state = states[i]
                    contract_info += ["\tState " + i + " is represented by the following variable assignments:\n"]
                    for var in state.keys() :
                        contract_info += ["\t\t" + var + " = " + state[var] + "\n"]
                state_info += [self.generate_result(contract_info)]

        return state_info