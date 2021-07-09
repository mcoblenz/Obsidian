from typing import *

class Edge:
    def __init__(self, s_from, s_to, label):
        self.s_from = s_from
        self.s_to = s_to
        self.label = label
    def __eq__(self, other):
        if isinstance(other, Edge):
            return hash(self) == hash(other)
        else:
            return False
    def __hash__(self):
        return hash((self.s_from, self.s_to, self.label))
    def __str__(self):
        return "%s --[%s]--> %s" % (self.s_from, self.label, self.s_to)

class TransitionGraph:
    def __init__(self, states, initial_states, edges = None):
        if edges is None: 
            edges = set()
        self.states: Set[str] = states.copy()
        self.initial_states: Set[str] = initial_states.copy()
        self.edges: Set[Edge] = edges.copy()
        self.adj: Dict[str, List[Tuple[str, str]]] = {s:[] for s in self.states}
        for e in self.edges:
            self.adj[e.s_from].append((e.s_to, e.label))

    def addEdge(self, s_from, s_to, label):
        self.edges.add(Edge(s_from,s_to,label))
        self.adj[s_from].append((s_to, label))

    def __str__(self):
        return '\n'.join(map(str,self.edges))
