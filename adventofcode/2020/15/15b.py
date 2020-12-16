from collections import defaultdict
import sys

seq = list(map(int, sys.stdin.read().split(',')))
history = defaultdict(list)

for i, n in enumerate(seq):
    history[n].append(i)

while len(seq) < 30000000:
    i += 1
    if len(history[seq[-1]]) == 1:
        n = 0
    else:
        n = history[seq[-1]][-1] - history[seq[-1]][-2]
    seq.append(n)
    history[n].append(i)

#print(history)
print(seq[-1])