import sys
from typing import List, Set, Tuple


def play_game(d1 : List[int], d2 : List[int], game_num : int) -> int:
    seen = set([(tuple(d1), tuple(d2))])
    round_num = 1
    while d1 and d2:
        play_round(d1, d2, game_num, round_num)
        round_num += 1
        if (tuple(d1), tuple(d2)) in seen:
            return 1
        else:
            seen.add((tuple(d1), tuple(d2)))
    return 1 if d1 else 2


def play_round(d1 : List[int], d2 : List[int], game_num : int, round_num : int):
    c1 = d1.pop(0)
    c2 = d2.pop(0)
    if c1 > len(d1) or c2 > len(d2):
        if c1 > c2:
            d1.append(c1) ; d1.append(c2)
        else:
            d2.append(c2) ; d2.append(c1)
    else:
        winner = play_game(list(d1[0:c1]), list(d2[0:c2]), game_num + 1)
        if winner == 1:
            d1.append(c1) ; d1.append(c2)
        else:
            d2.append(c2) ; d2.append(c1)


p1, p2 = [], []

sys.stdin.readline()
while l := sys.stdin.readline().strip():
    p1.append(int(l))

sys.stdin.readline()
while l:= sys.stdin.readline().strip():
    p2.append(int(l))

play_game(p1, p2, 1)

winner = p1 or p2
print(sum(map(lambda x: x[0] * x[1], (zip(winner, range(len(winner), 0, -1))))))