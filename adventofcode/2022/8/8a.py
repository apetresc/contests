import itertools
import sys

g = [list(map(int, l.strip())) for l in sys.stdin.readlines()]
N, M = len(g), len(g[0])
o = 0

for i, j in itertools.product(range(1, N-1), range(1, M-1)):
    if g[i][j] > max(g[i][:j]) or g[i][j] > max(g[i][j+1:]) or g[i][j] > max([g[k][j] for k in range(i)]) or g[i][j] > max(g[k][j] for k in range(i+1, N)):
        o += 1

print(N + N + M + M - 4 + o)
