from collections import defaultdict

N = int(input())
Cs = [input().split() for _ in range(N)]
Ps = defaultdict(int)

def hash(C):
    return ''.join(sorted(C))

for C in Cs:
    Ps[hash(C)] += 1

most_popular = max(Ps.values())
PPs = [k for k, v in Ps.items() if v == most_popular]
print(len([C for C in Cs if hash(C) in PPs]))

