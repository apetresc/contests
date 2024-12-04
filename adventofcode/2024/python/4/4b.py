import sys
from itertools import product


def check_dir(
    grid: list[list[str]], target: str, x: int, y: int, xd: int, yd: int
) -> bool:
    l = len(target) // 2
    if not ((l <= x < len(grid) - l) and (l <= y < len(grid[x]) - l)):
        return False
    if "".join([grid[x + xd * i][y + yd * i] for i in range(-l, l + 1)]) == target:
        return True
    return False


def check(grid: list[list[str]], target: str, x: int, y: int) -> int:
    return (
        sum(
            [
                check_dir(grid, target, x, y, xd, yd)
                for (xd, yd) in product([1, -1], repeat=2)
            ]
        )
        >= 2
    )


def search(grid: list[list[str]], target: str) -> int:
    if len(target) % 2 != 1:
        return 0
    total = 0
    for x in range(len(grid)):
        for y in range(len(grid[x])):
            total += check(grid, target, x, y)
    return total


def main():
    grid = list(map(lambda l: list(l.strip()), open(sys.argv[1], "r").readlines()))
    return search(grid, "MAS")


if __name__ == "__main__":
    print(main())
