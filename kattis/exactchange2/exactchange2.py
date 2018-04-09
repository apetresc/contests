from collections import defaultdict
from copy import copy
N = int(input())

for _ in range(N):
    P = int(input())
    C = int(input())
    Cs = [int(input()) for _ in range(C)]
    Css = {
        k: Cs.count(k)
        for k in set(Cs)
    }
    dp = defaultdict(list)

    #print(Cs)

    for c in set(Cs):
        dp[c].append(c)

    #print("nonempties: %s" % {k: dp[k] for k in dp.keys()})

    for _ in range(C-1):
        news = {}
        for v in dp.keys():
            #print("v=%d" % v)
            for x in [c for c in Cs if Css[c] > dp[v].count(c)]:
                if (sum(dp[v]) + x) not in dp  or len(dp[sum(dp[v]) + x]) > len(dp[v]):
                    #print("\tx=%d"%x)
                    news[sum(dp[v]) + x] = dp[v] + [x]
        for n1, n2 in news.items():
            dp[n1] = n2
        #print("nonempties: %s" % {k: dp[k] for k in dp.keys()})
        if P in dp.keys():
            break

    answer = min([v for v in dp.keys() if v >= P])
print(answer, len(dp[answer]))
