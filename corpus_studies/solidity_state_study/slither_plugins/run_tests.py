from .ContractStateDetector import ContractStateDetector
from ..tests.set_solc_version import set_version
from .EnumStateDetector import inferTransitionGraph
import glob
import pytest

from .Graph import Edge, TransitionGraph
from slither import Slither

# a has Functions as labels, while b has strings as labels
# For testing purposes, we compare the edges by turning the functions into the function names.
def equal_graph(a: TransitionGraph, b: TransitionGraph) -> bool:
    return set(Edge(e.s_from,e.s_to,e.label.name) for e in a.edges) == b.edges and a.states == b.states and a.initial_states == b.initial_states

# To run tests, run pytest run_tests.py

# We assume we are checking the last contract in the file.
# For those in the state directory, we expect the last contract to be detected with state.
# For those in the nostate directory, we expect the last contract to not be detected with state.

state_files = glob.glob("tests/state/*.sol")
@pytest.mark.parametrize('f', state_files)
def test_state(f):
    set_version(f)
    slither = Slither(f)
    c = slither.contracts[-1]
    assert(len(ContractStateDetector(c).states_in_contract()) > 0)

nostate_files = glob.glob("tests/nostate/*.sol")
@pytest.mark.parametrize('f', nostate_files)
def test_nostate(f):
    set_version(f)
    slither = Slither(f)
    c = slither.contracts[-1]
    assert(len(ContractStateDetector(c).states_in_contract()) == 0)

# Individual tests to check that the state transition graphs have been created correctly.
def test_BranchingStates1():
    file = "tests/enums/BranchingStates1.sol"
    set_version(file)
    slither = Slither(file)
    c = slither.contracts[0]
    (_, graph) = inferTransitionGraph(c)
    expGraph = TransitionGraph(states={"A","B","C"}, 
                               initial_states={"B"},
                               edges={
                                   Edge("A","A","moveToB"),
                                   Edge("A","B","moveToB")
                               })
    assert(equal_graph(graph, expGraph))

def test_BranchingStates2():
    file = "tests/enums/BranchingStates2.sol"
    set_version(file)
    slither = Slither(file)
    c = slither.contracts[0]
    (_, graph) = inferTransitionGraph(c)
    expGraph = TransitionGraph(states={"A","B","C"}, 
                               initial_states={"A"},
                               edges={
                                   Edge("B","C","moveToC")
                               })
    assert(equal_graph(graph, expGraph))

# test_BranchingStates2()