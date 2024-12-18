import sys
from collections import defaultdict


def blink(stones: list[int]) -> list[int]:
    new_stones = []
    for stone in stones:
        if stone == 0:
            new_stones.append(1)
        elif len(str(stone)) % 2 == 0:
            new_stones.append(int(str(stone)[0 : len(str(stone)) // 2]))
            new_stones.append(int(str(stone)[len(str(stone)) // 2 :]))
        else:
            new_stones.append(stone * 2024)
    return new_stones


def a(stones: list[int], blinks: int) -> int:
    for _ in range(blinks):
        stones = blink(stones)
    return len(stones)


def b(stones: list[int], blinks: int) -> int:
    counts = defaultdict(int)

    for stone in stones:
        counts[stone] += 1

    for _ in range(blinks):
        new_counts = defaultdict(int)
        for n, c in counts.items():
            if n == 0:
                new_counts[1] += c
            elif len(str(n)) % 2 == 0:
                new_counts[int(str(n)[0 : len(str(n)) // 2])] += c
                new_counts[int(str(n)[len(str(n)) // 2 :])] += c
            else:
                new_counts[n * 2024] += c

        counts = new_counts

    return sum(counts.values())


def parse_input(path: str) -> list[int]:
    return list(map(int, open(path, "r").readline().strip().split(" ")))


if __name__ == "__main__":
    stones = parse_input(sys.argv[1])
    print(a(stones, 25))
    print(b(stones, 75))
