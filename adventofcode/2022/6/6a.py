import sys

print((lambda L: [len(set(L[i:i+4])) for i in range(len(L)-4)])(sys.stdin.readline()).index(4) + 4)
