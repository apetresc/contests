import sys
from itertools import product


def check_dir(
    grid: list[list[str]], target: str, x: int, y: int, dx: int, dy: int
) -> bool:
    l = len(target) // 2
    if not ((l <= x < len(grid) - l) and (l <= y < len(grid[0]) - l)):
        return False
    return "".join([grid[x + dx * i][y + dy * i] for i in range(-l, l + 1)]) == target


def check(grid: list[list[str]], target: str, x: int, y: int) -> int:
    return (
        sum(
            check_dir(grid, target, x, y, dx, dy)
            for (dx, dy) in product([1, -1], repeat=2)
        )
        >= 2
    )


def search(grid: list[list[str]], target: str) -> int:
    if len(target) % 2 != 1:
        return 0
    return sum(
        check(grid, target, x, y) for x in range(len(grid)) for y in range(len(grid[0]))
    )


def main():
    grid = list(map(lambda l: list(l.strip()), open(sys.argv[1], "r").readlines()))
    return search(grid, "MAS")


if __name__ == "__main__":
    print(main())
