from collections import defaultdict

edges = defaultdict(list)
root = None

while True:
    try:
        orbitee, orbiter = input().split(")")
        if orbiter == "YOU":
            root = orbitee
        else:
            edges[orbiter].append(orbitee)
            edges[orbitee].append(orbiter)
    except EOFError:
        break

print(edges)
print(root)

def bfs(edges, root):
    seen = {root,}
    q = [(root, []),]
    while q:
        node, path = q.pop()
        print("Visiting node %s" % node)
        seen.add(node)
        if node == "SAN":
            return path
        for neighbor in edges[node]:
            if neighbor not in seen:
                q.append((neighbor, path + [node]))

print(len(bfs(edges, root)) - 1)
