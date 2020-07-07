from slither.detectors.abstract_detector import AbstractDetector, DetectorClassification


class HasState(AbstractDetector):
    ARGUMENT = 'hasstate'
    HELP = 'Help printed by slither'
    IMPACT = DetectorClassification.HIGH
    CONFIDENCE = DetectorClassification.HIGH

    WIKI = 'STATE TEST'

    WIKI_TITLE = 'State Detection Tool'
    WIKI_DESCRIPTION = 'Detects whether or not a contract can be said to have multiple states'
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
    
    In the above contract, there are two states: Paused and Unpaused. The classifier would identify them as
    Paused and NotPaused, because the requires statements use paused and !paused as guards. 
    '''

    WIKI_RECOMMENDATION = 'It\'s fine to be stateful! Don\'t change anything'

    def inherited_state(self, contract, stateful) :
        return [elem for elem in contract.inheritance if elem in stateful]


    def is_stateful_node(self, node) :
        if node.is_conditional() :
            return [bool(node.state_variables_read) and not bool(node.local_variables_read)]
        return [False]

    def is_stateful_function(self, func) :
        parameters = func.parameters
        nodes = func._explore_func_nodes(func, self.is_stateful_node)
        return True in nodes

    def is_stateful_contract(self, contract) :
        state_vars = contract.state_variables
        for sv in state_vars :
            for m in contract.modifiers :        
                if self.is_stateful_function(m) :
                    return True
            for f in contract.functions :
                if self.is_stateful_function(f) :
                    return True
        return False

    def _detect(self):
        stateful_contracts = []
        for c in self.contracts :
            if self.is_stateful_contract(c) or self.inherited_state(c, stateful_contracts):
                stateful_contracts.append(c.name + " contains state \n")
        if stateful_contracts :
            return [self.generate_result(stateful_contracts)]
        return []
