from collections import defaultdict

edges = defaultdict(list)

while True:
    try:
        orbitee, orbiter = input().split(")")
        edges[orbiter].append(orbitee)
    except EOFError:
        break

def count_orbits(node):
    if node == 'COM':
        return 0
    else:
        return 1 + sum([count_orbits(n) for n in edges[node]])

print(sum(map(count_orbits, edges.keys())))
