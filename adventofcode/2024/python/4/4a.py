import sys
from itertools import product


def check_dir(
    grid: list[list[str]], target: str, x: int, y: int, dx: int, dy: int
) -> bool:
    if grid[x][y] != target[0]:
        return False
    if (x + (len(target) - 1) * dx not in range(len(grid))) or (
        y + (len(target) - 1) * dy not in range(len(grid[0]))
    ):
        return False
    return "".join([grid[x + dx * i][y + dy * i] for i in range(len(target))]) == target


def check(grid: list[list[str]], target: str, x: int, y: int) -> int:
    return sum(
        check_dir(grid, target, x, y, dx, dy)
        for (dx, dy) in product([1, 0, -1], repeat=2)
        if (dx, dy) != (0, 0)
    )


def search(grid: list[list[str]], target: str) -> int:
    return sum(
        check(grid, target, x, y) for x in range(len(grid)) for y in range(len(grid[0]))
    )


def main():
    grid = list(map(lambda l: list(l.strip()), open(sys.argv[1], "r").readlines()))
    return search(grid, "XMAS")


if __name__ == "__main__":
    print(main())
