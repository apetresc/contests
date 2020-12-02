import re
import sys


valid = 0

for line in sys.stdin:
    m = re.search(r'(\d+)-(\d+) ([a-z]): ([a-z]+)', line)
    l, u, c, p = m.groups()
    if int(l) <= p.count(c) <= int(u):
        valid += 1
print(valid)
