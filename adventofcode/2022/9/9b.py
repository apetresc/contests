import sys


def sgn(x):
    if x >= 0:
        return 1
    else:
        return -1

D = {'R': (+1,0), 'L': (-1,0), 'U': (0,+1), 'D': (0, -1)}
L = 10
p = [(0,0) for _ in range(10)]
s = set([(0,0)])

for d, v in map(lambda l: l.split(), sys.stdin.readlines()):
    d = D[d]
    for _ in range(int(v)):
        p[0] = tuple(x + y for x, y in zip(p[0], d))
        for i in range(1, L):
            delta = tuple(x - y for x,y in zip(p[i], p[i-1]))
            if max(map(abs, delta)) > 1:
                if delta[0] and delta[1]:
                    # diagonal move
                    p[i] = tuple(x - sgn(y) for x, y in zip(p[i], delta))
                else:
                    # lateral move, tail just needs to follow
                    p[i] = tuple(x + (y - x) // 2 for x, y in zip(p[i], p[i-1]))
        s.add(p[9])

print(len(s))
