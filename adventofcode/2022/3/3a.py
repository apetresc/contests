import sys

print(sum(map(lambda c: ord(c) - (96 if "a"<=c<="z" else 38), map(lambda l: set(l[:len(l)//2]).intersection(set(l[len(l)//2:])).pop(), sys.stdin.readlines()))))
