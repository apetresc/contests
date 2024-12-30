import sys
from itertools import batched

def parse_input(path: str) -> str:
    return open(path, "r").readline().strip()

POTIONS_REQUIRED = {
    'A': 0,
    'B': 1,
    'C': 3,
    'D': 5,
    'x': 0
}

def a(battles: str) -> int:
    potions = 0
    for battle in battles:
        potions += POTIONS_REQUIRED[battle]

    return potions

def b(battles: str) -> int:
    potions = 0
    for a, b in batched(battles, 2):
        pair = 0 if 'x' in (a,b) else 2
        potions += POTIONS_REQUIRED[a] + POTIONS_REQUIRED[b] + pair
    return potions

def c(battles: str) -> int:
    potions = 0
    for a, b, c in batched(battles, 3):
        bonus = max(0, 6 - 4 * (a,b,c).count('x'))
        potions += POTIONS_REQUIRED[a] + POTIONS_REQUIRED[b] + POTIONS_REQUIRED[c] +  bonus
    return potions


if __name__ == "__main__":
    battles = parse_input(sys.argv[1])
    print(a(battles))
    print(b(battles))
    print(c(battles))
