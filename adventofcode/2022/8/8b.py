import itertools
import sys

g = [list(map(int, l.strip())) for l in sys.stdin.readlines()]
N, M = len(g), len(g[0])
S = []

for i, j in itertools.product(range(1, N-1), range(1, M-1)):
    s = 1
    for dx, dy in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
        v = 0
        x, y = i+dx, j+dy
        while x in range(0, N) and y in range(0, M):
            v += 1
            if g[x][y] >= g[i][j]:
                break
            x, y = x+dx, y+dy
        s *= v
    S.append(s)

print(max(S))
