import sys
from itertools import product


def check_dir(
    grid: list[list[str]], target: str, x: int, y: int, xd: int, yd: int
) -> bool:
    if grid[x][y] != target[0]:
        return False
    if (x + (len(target) - 1) * xd) not in range(len(grid)) or (
        y + (len(target) - 1) * yd
    ) not in range(len(grid[x])):
        return False
    if "".join([grid[x + xd * i][y + yd * i] for i in range(len(target))]) == target:
        return True
    return False


def check(grid: list[list[str]], target: str, x: int, y: int) -> int:
    return sum(
        [
            check_dir(grid, target, x, y, xd, yd)
            for (xd, yd) in product([1, 0, -1], repeat=2)
        ]
    )


def search(grid: list[list[str]], target: str) -> int:
    total = 0
    for x in range(len(grid)):
        for y in range(len(grid[x])):
            total += check(grid, target, x, y)
    return total


def main():
    grid = list(map(lambda l: list(l.strip()), open(sys.argv[1], "r").readlines()))
    return search(grid, "XMAS")


if __name__ == "__main__":
    print(main())
