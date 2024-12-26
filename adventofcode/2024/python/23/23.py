import sys
from collections import defaultdict
from typing import Optional

Graph = dict[str, list[str]]


def bron_kerbosch(graph: Graph, clique: Optional[set[str]]=None, candidates:
                  Optional[set[str]]=None, excluded: Optional[set[str]]=None):
    """
    Find all maximal cliques in an undirected graph.

    Args:
        graph: Dict mapping nodes to sets of their neighbors
        clique: Current clique being built
        candidates: Set of vertices that could extend the clique
        excluded: Set of vertices that have already been processed

    Returns:
        List of all maximal cliques (each clique is a set of vertices)
    """
    # Initialize default parameters for the first call
    if clique is None:
        clique = set()
    if candidates is None:
        candidates = set(graph.keys())
    if excluded is None:
        excluded = set()

    results = []

    # Base case: if both candidates and excluded are empty,
    # we've found a maximal clique
    if not candidates and not excluded:
        results.append(clique.copy())
        return results

    while candidates:
        # Choose a potential vertex to add to our clique
        v = candidates.pop()

        # Find new candidates and excluded sets for recursive call
        # New candidates must be neighbors of v and current candidates
        new_candidates = candidates.intersection(graph[v])
        # New excluded must be neighbors of v and current excluded
        new_excluded = excluded.intersection(graph[v])

        # Make recursive call with:
        # - expanded clique (add v)
        # - new candidates (neighbors of v that were candidates)
        # - new excluded (neighbors of v that were excluded)
        results.extend(bron_kerbosch(
            graph,
            clique.union({v}),
            new_candidates,
            new_excluded
        ))

        # Move v to excluded for future iterations
        excluded.add(v)

    return results


def triples(network: Graph) -> list[tuple[str, str, str]]:
    cliques = set()
    for computer, neighbors in network.items():
        if computer.startswith('t'):
            for neighbor in neighbors:
                for neighbor_neighbor in network[neighbor]:
                    if neighbor_neighbor in neighbors:
                        cliques.add(tuple(sorted((computer, neighbor,
                                                  neighbor_neighbor))))
    return list(cliques)


def a(network: Graph) -> int:
    cliques = triples(network)
    return len(cliques)

def b(network: Graph) -> str:
    cliques = bron_kerbosch(network)
    maximal_clique_size = max(len(g) for g in cliques)
    maximal_cliques = [c for c in cliques if len(c) == maximal_clique_size]
    return ",".join(sorted(maximal_cliques[0]))


def parse_input(path: str) -> Graph:
    network = defaultdict(list)
    for l in open(path, "r").readlines():
        a, b = l.strip().split("-")
        network[a].append(b)
        network[b].append(a)
    return network

if __name__ == "__main__":
    network = parse_input(sys.argv[1])
    print(a(network))
    print(b(network))
