import sys
from typing import List


def play_round(d1 : List[int], d2 : List[int]):
    if d1[0] > d2[0]:
        d1.append(d1.pop(0))
        d1.append(d2.pop(0))
    elif d2[0] > d1[0]:
        d2.append(d2.pop(0))
        d2.append(d1.pop(0))

p1, p2 = [], []

sys.stdin.readline()
while l := sys.stdin.readline().strip():
    p1.append(int(l))

sys.stdin.readline()
while l:= sys.stdin.readline().strip():
    p2.append(int(l))

while p1 and p2:
    play_round(p1, p2)

winner = p1 or p2
print(sum(map(lambda x: x[0] * x[1], (zip(winner, range(len(winner), 0, -1))))))