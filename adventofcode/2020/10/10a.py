import sys

adapters = sorted(map(int, sys.stdin)) 
adapters = [0] + adapters + [3 + max(adapters)]
diffs = list(map(lambda x: x[1] - x[0], zip(adapters, adapters[1:])))
print(adapters)
print(diffs)
print(diffs.count(1), diffs.count(2), diffs.count(3))