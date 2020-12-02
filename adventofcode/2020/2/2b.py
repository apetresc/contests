import re
import sys


valid = 0

for line in sys.stdin:
    m = re.search(r'(\d+)-(\d+) ([a-z]): ([a-z]+)', line)
    l, u, c, p = m.groups()
    if (p[int(l)-1] == c) != (p[int(u)-1] == c):
        valid += 1
print(valid)
