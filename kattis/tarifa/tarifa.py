X = int(input())
N = int(input())
P = [int(input()) for p in range(N)]

print(X + sum([X - P[i] for i in range(N)]))
