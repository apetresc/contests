import sys

m = set()

for line in sys.stdin:
    n = int(line)
    target = 2020 - n
    if target in m:
        print((target) * n)
    else:
        m.add(n)