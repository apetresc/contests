import sys

l = list(map(int, sys.stdin))

for i in range(len(l)):
    m = set()
    target = 2020 - l[i]
    for j in range(i + 1, len(l)):
        if target - l[j] in m:
            print(l[i] * l[j] * (target - l[j]))
        else:
            m.add(l[j])