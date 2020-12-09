import sys

PREAMBLE_SIZE = 25


def find_sum(l, s):
    seen = set()
    for n in l:
        if s - n in seen:
            return True
        seen.add(n)
    return False

l = list(map(int, sys.stdin))
for i in range(PREAMBLE_SIZE, len(l)):
    #print(i, l[i], l[i-PREAMBLE_SIZE:i])
    if not find_sum(l[i-PREAMBLE_SIZE:i], l[i]):
        print(l[i])
        sys.exit(0)