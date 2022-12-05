import sys

print(sum(map(lambda l: (ord(l[2]) - 87) + ((ord(l[0]) - (ord(l[2]) - 23)) * 2 + 1) % 3 * 3, sys.stdin.readlines())))
