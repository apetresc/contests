T = int(input())

def trouble_sort(L):
    done = False
    while not done:
        done = True
        for i in range(len(L) - 2):
            if L[i] > L[i + 2]:
                done = False
                L[i], L[i + 2] = L[i + 2], L[i]
    return L

for i in range(1, T+1):
    N = int(input())
    V = list(map(int, input().split()))

    V1, V2 = sorted(V[::2]), sorted(V[1::2])
    VT = V1 + V2
    VT[::2] = V1
    VT[1::2] = V2
    VS = sorted(VT)
    if False:
        TS = trouble_sort(V)
        print("V =", V)
        print("V1 =", V1)
        print("V2 =", V2)
        print("VS =", VS)
        print("VT =", VT)
        print("TS =", TS)
        if VT != TS:
            print("!!!!!!!!!!!!!!!!!!!!!!!!!ALERT!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

    if VS == VT:
        print("Case #%d: %s" % (i, "OK"))
    else:
        print("Case #%d: %d" % (i, min([i for i in range(len(VT)) if VS[i] != VT[i]])))
