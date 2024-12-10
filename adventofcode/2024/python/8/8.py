import sys
from collections import defaultdict
from itertools import product


def extract_antenna(grid: list[list[str]]) -> dict[str, list[tuple[int, int]]]:
    antenna = defaultdict(list)
    for r in range(len(grid)):
        for c in range(len(grid[0])):
            if grid[r][c] != ".":
                antenna[grid[r][c]].append((r, c))
    return antenna


def a(grid: list[list[str]]) -> int:
    antenna = extract_antenna(grid)
    antinodes = set()

    for freq in antenna.keys():
        for pair in product(antenna[freq], repeat=2):
            if pair[0] == pair[1]:
                continue
            dx, dy = (pair[0][0] - pair[1][0], pair[0][1] - pair[1][1])
            antinode = (pair[0][0] + dx, pair[0][1] + dy)
            if 0 <= antinode[0] < len(grid) and 0 <= antinode[1] < len(grid[0]):
                antinodes.add(antinode)
    display_grid(grid, list(antinodes))
    return len(antinodes)


def b(grid: list[list[str]]) -> int:
    antenna = extract_antenna(grid)
    antinodes = set()

    for freq in antenna.keys():
        for pair in product(antenna[freq], repeat=2):
            if pair[0] == pair[1]:
                antinodes.add(pair[0])
                continue
            dx, dy = (pair[0][0] - pair[1][0], pair[0][1] - pair[1][1])
            while 0 <= (pair[0][0] + dx) < len(grid) and 0 <= (pair[0][1] + dy) < len(
                grid[0]
            ):
                antinodes.add((pair[0][0] + dx, pair[0][1] + dy))
                dx += pair[0][0] - pair[1][0]
                dy += pair[0][1] - pair[1][1]
    display_grid(grid, list(antinodes))

    return len(antinodes)


def display_grid(grid: list[list[str]], antinodes: list[tuple[int, int]] = []) -> None:
    for r, row in enumerate(grid):
        for c, char in enumerate(row):
            if (r, c) in antinodes:
                print("#", end="")
            else:
                print(char, end="")
        print("")
    print("\n\n")


def parse_input(
    input_path: str,
) -> list[list[str]]:
    with open(input_path, "r") as f:
        grid = [[c for c in line.strip()] for line in f.readlines()]

    return grid


if __name__ == "__main__":
    grid = parse_input(sys.argv[1])
    print(a(grid))
    print(b(grid))
