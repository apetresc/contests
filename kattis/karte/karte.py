S = input()
N = len(S) // 3

C = {'P': set(), 'K': set(), 'H': set(), 'T': set()}

for i in range(N):
    c = S[i*3:(i + 1)*3]
    C[c[0]].add(c)

if sum([len(v) for k, v in C.items()]) != N:
    print("GRESKA")
else:
    print(' '.join([str(13 - len(C[x])) for x in ['P', 'K', 'H', 'T']]))

