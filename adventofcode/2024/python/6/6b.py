import copy
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
    try:
        while grid[pos[0]][pos[1]] != "#":
            seen[pos].add(dir)
            pos = pos[0] + dir[0], pos[1] + dir[1]
    except IndexError:
        return False
    return (pos[0] - dir[0], pos[1] - dir[1])


def check_loop(grid, pos, dir) -> bool:
    seen: dict[tuple[int, int], set[tuple[int, int]]] = defaultdict(set)
    seen_count = 0
    while seen_count < 3:
        while dir not in seen[pos]:
            pos = go(grid, pos, dir, seen)
            if not pos:
                return False
            dir = turn(dir)
        seen_count += 1

    display_grid(grid, seen)
    print("\n\n\n")
    return True


def main():
    with open(sys.argv[1], "r") as f:
        grid = [[c for c in line.strip()] for line in f.readlines()]
    pos = (0, 0)
    dir: tuple[int, int] = (0, 0)

    for r in range(len(grid)):
        for c in range(len(grid[r])):
            if grid[r][c] in "<>^v":
                pos = (r, c)
                match grid[r][c]:
                    case ">":
                        dir = (1, 0)
                    case "v":
                        dir = (0, -1)
                    case "<":
                        dir = (-1, 0)
                    case "^":
                        dir = (-1, 0)
                    case _:
                        dir = (0, 0)
                grid[r][c] = "."
                print(pos, dir)
                break
        else:
            continue

    total = 0
    for r in range(len(grid)):
        for c in range(len(grid[r])):
            if (r, c) != pos and grid[r][c] == ".":
                possible_obstruction = copy.deepcopy(grid)
                possible_obstruction[r][c] = "#"
                if check_loop(possible_obstruction, pos, dir):
                    total += 1

    return total


if __name__ == "__main__":
    print(main())
