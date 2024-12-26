import sys

from collections import Counter
from itertools import islice, pairwise
from typing import Generator

BASE = 16777216

def gen_secrets(seed: int) -> Generator[int]:
    secret_number = seed

    while True:
        yield secret_number
        secret_number = (secret_number ^ (secret_number * 64)) % BASE
        secret_number = (secret_number ^ (secret_number // 32)) % BASE
        secret_number = (secret_number ^ (secret_number * 2048)) % BASE

def a(sellers: list[int]) -> int:
    total = 0
    for seller in sellers:
        total += next(islice(gen_secrets(seller), 2000, 2001))

    return total

def b(sellers: list[int]) -> int:
    seqs = Counter()
    for seller in sellers:
        first_seqs = {}
        sell_prices = [p % 10 for p in islice(gen_secrets(seller), 0, 2000)]
        assert len(sell_prices) == 2000
        deltas = list(map(lambda x: x[1] - x[0], pairwise(sell_prices)))
        for j in range(len(deltas) - 3):
            if tuple(deltas[j:j+4]) not in first_seqs:
                first_seqs[tuple(deltas[j:j+4])] = sell_prices[j+4]
        for k, v in first_seqs.items():
            seqs[k] += v

    return seqs.most_common(1)[0][1]

def parse_input(path: str) -> list[int]:
    return [int(l) for l in open(path, "r").readlines()]

if __name__ == "__main__":
    sellers = parse_input(sys.argv[1])
    print(a(sellers))
    print(b(sellers))
