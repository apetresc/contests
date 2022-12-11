import sys

D = {'R': (+1,0), 'L': (-1,0), 'U': (0,+1), 'D': (0, -1)}
p = [(0,0), (0,0)]
s = set([(0,0)])

for d, v in map(lambda l: l.split(), sys.stdin.readlines()):
    d = D[d]
    for _ in range(int(v)):
        p[0] = tuple(x + y for x, y in zip(p[0], d))
        delta = tuple(abs(x-y) for x,y in zip(p[0], p[1]))
        if max(delta) > 1:
            if delta[0] and delta[1]:
                # diagonal move
                p[1] = tuple(x - y for x, y in zip(p[0], d))
            else:
                # lateral move, tail just needs to follow
                p[1] = tuple(x + y for x, y in zip(p[1], d))
            # if sum(abs(x-y) for x,y in zip())
            s.add(p[1])

print(s)
print(len(s))
