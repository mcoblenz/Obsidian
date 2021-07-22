from slither.core.expressions import *
from slither.core.cfg.node import *
from slither.core.declarations import *

# Checks whether variables in a contract could be declared immutable
# (i.e., if they are never written to outside of the constructor/when they are initialized)
class ImmutableChecker:
    def __init__(self, contract: Contract):
        self.contract: Contract = contract
        self.poss_immutable: Set[StateVariable] = set()
        for var in contract.state_variables:
            funcs = [func for func in contract.functions_and_modifiers if not func.is_constructor and not func.is_constructor_variables]
            if all(var not in func.variables_written for func in funcs):
                self.poss_immutable.add(var)

    def could_be_immutable(self, var: StateVariable):
        return var in self.poss_immutable