import sys

print(sum(map(lambda l: 3 * (ord(l[2]) - 88) + (ord(l[0]) + ord(l[2]) - 154) % 3 + 1, sys.stdin.readlines())))
