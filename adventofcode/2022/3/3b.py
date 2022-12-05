import sys
import itertools
import functools

print(sum(map(lambda c: ord(list(c)[0]) - (96 if "a"<=list(c)[0]<="z" else 38), (map(lambda ls: functools.reduce(lambda u,v: set(u).intersection(set(v)), ls), map(lambda x: [y[1].strip() for y in x[1]], itertools.groupby(enumerate(sys.stdin.readlines()), lambda x: x[0]//3)))))))
