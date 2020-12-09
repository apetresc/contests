import itertools
import sys

PREAMBLE_SIZE = 25


def find_sum(l, s):
    seen = set()
    for n in l:
        if s - n in seen:
            return True
        seen.add(n)
    return False

def find_contiguous_sum(l, s):
    for pair in itertools.combinations(range(len(l)), 2):
        if sum(l[pair[0]:pair[1]]) == s:
            print(min(l[pair[0]:pair[1]]) + max(l[pair[0]:pair[1]]))
            return

l = list(map(int, sys.stdin))
for i in range(PREAMBLE_SIZE, len(l)):
    if not find_sum(l[i-PREAMBLE_SIZE:i], l[i]):
        find_contiguous_sum(l[0:i], l[i])
        sys.exit(0)