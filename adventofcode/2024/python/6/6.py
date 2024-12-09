import sys
from collections import defaultdict
from typing import Literal, Optional


def turn(dir: tuple[int, int]) -> tuple[int, int]:
    match dir:
        case (1, 0):
            return (0, -1)
        case (0, -1):
            return (-1, 0)
        case (-1, 0):
            return (0, 1)
        case (0, 1):
            return (1, 0)
    raise ValueError("Invalid direction:", dir)


def display_grid(
    grid: list[list[str]],
    seen: Optional[dict[tuple[int, int], set[tuple[int, int]]]] = None,
) -> None:
    for r, row in enumerate(grid):
        for c, char in enumerate(row):
            if seen:
                match len(seen.get((r, c)) or []):
                    case 0:
                        print(char, end="")
                    case 1:
                        print("-" if next(iter(seen[(r, c)]))[0] == 0 else "|", end="")
                    case 2:
                        print("+", end="")
            else:
                print(char, end="")
        print("")


def go(
    grid, pos: tuple[int, int], dir: tuple[int, int], seen
) -> tuple[int, int] | Literal[False]:
    while (
        0 <= pos[0] < len(grid)
        and 0 <= pos[1] < len(grid[0])
        and grid[pos[0]][pos[1]] != "#"
    ):
        seen[pos].add(dir)
        pos = pos[0] + dir[0], pos[1] + dir[1]
    if pos[0] < 0 or pos[1] < 0 or pos[0] >= len(grid) or pos[1] >= len(grid[0]):
        return False
    return (pos[0] - dir[0], pos[1] - dir[1])


def check_loop(grid, pos, dir) -> bool:
    seen: dict[tuple[int, int], set[tuple[int, int]]] = defaultdict(set)
    while dir not in seen[pos]:
        pos = go(grid, pos, dir, seen)
        if not pos:
            return False
        dir = turn(dir)

    display_grid(grid, seen)
    print("\n\n\n")
    return True


def a(
    grid: list[list[str]], initial_pos: tuple[int, int], initial_dir: tuple[int, int]
):
    seen: dict[tuple[int, int], set[tuple[int, int]]] = defaultdict(set)
    pos = initial_pos
    dir = initial_dir

    while dir not in seen[pos]:
        pos = go(grid, pos, dir, seen)
        if not pos:
            break
        dir = turn(dir)
        display_grid(grid, seen)
        print("\n\n\n")

    display_grid(grid, seen)
    return len([p for p in seen if seen[p]])


def b(grid: list[list[str]], pos: tuple[int, int], dir: tuple[int, int]) -> int:
    total = 0
    for r in range(len(grid)):
        for c in range(len(grid[r])):
            if (r, c) != pos and grid[r][c] == ".":
                grid[r][c] = "#"
                if check_loop(grid, pos, dir):
                    total += 1
                grid[r][c] = "."
    return total


def parse_input(
    input_path: str,
) -> tuple[list[list[str]], tuple[int, int], tuple[int, int]]:
    with open(input_path, "r") as f:
        grid = [[c for c in line.strip()] for line in f.readlines()]
    pos = (0, 0)
    dir: tuple[int, int] = (0, 0)

    for r in range(len(grid)):
        for c in range(len(grid[r])):
            if grid[r][c] in "^":
                pos = (r, c)
                dir = (-1, 0) # I'm assuming we always start facing up
                grid[r][c] = "."
                break
        else:
            continue

    return grid, pos, dir


if __name__ == "__main__":
    grid, pos, dir = parse_input(sys.argv[1])
    print(a(grid, pos, dir))
    print(b(grid, pos, dir))
