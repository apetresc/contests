import sys

print((lambda L: [len(set(L[i:i+14])) for i in range(len(L)-14)])(sys.stdin.readline()).index(14) + 14)
