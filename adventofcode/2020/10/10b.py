from itertools import groupby, zip_longest
from collections import defaultdict
import sys

adapters = sorted(map(int, sys.stdin)) 
#adapters = [0] + adapters + [3 + max(adapters)]
diffs = list(map(lambda x: x[1] - x[0], zip(adapters, adapters[1:])))
print(adapters)
print(diffs)

combinations = defaultdict(int, {0: 1})
for adapter in adapters:
    combinations[adapter] = combinations[adapter-1] + combinations[adapter-2] + combinations[adapter-3]
print(combinations[adapters[-1]])
print(combinations)