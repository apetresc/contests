def evaluate(P):
    s = 1
    d = 0
    for c in P:
        if c == 'S':
            d += s
        elif c == 'C':
            s += s
    #print("Evaluating %s to %d" % (P, d))
    return d

T = int(input())
for i in range(1, T+1):
    D, P = input().split(' ')
    D = int(D)
    h = 0

    while evaluate(P) > D and "CS" in P:
        h += 1
        P = P[0:P.rfind("CS")] + "SC" + P[P.rfind("CS") + 2:]

    if evaluate(P) <= D:
        print("Case #%d: %d" % (i, h))
    else:
        print("Case #%d: IMPOSSIBLE" % i)
