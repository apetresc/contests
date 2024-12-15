import re
import sys
from copy import deepcopy
from dataclasses import dataclass


@dataclass
class Pos:
    x: int
    y: int


@dataclass
class Robot:
    p: Pos
    v: Pos


def tick(robots: list[Robot], t: int, W=101, H=103) -> list[Robot]:
    for robot in robots:
        robot.p = Pos((robot.p.x + t * robot.v.x) % W, (robot.p.y + t * robot.v.y) % H)
    return robots


def a(robots: list[Robot], W=101, H=103, T=100) -> int:
    quadrants = [[0, 0], [0, 0]]
    robots = tick(robots, T, W, H)
    for robot in robots:
        if robot.p.x != W // 2 and robot.p.y != H // 2:
            quadrants[robot.p.x // (W // 2 + 1)][robot.p.y // (H // 2 + 1)] += 1
    print(quadrants)

    return quadrants[0][0] * quadrants[0][1] * quadrants[1][0] * quadrants[1][1]


def b(robots: list[Robot], W=101, H=103) -> int:
    t = 0
    while True:
        robots = tick(robots, 1, W, H)
        t += 1
        if check_tree(robots, W, H):
            display_grid(robots)
            return t


def check_tree(robots: list[Robot], W=101, H=103):
    grid = [[0 for _ in range(W)] for _ in range(H)]
    for robot in robots:
        grid[robot.p.y][robot.p.x] += 1
        if grid[robot.p.y][robot.p.x] > 1:
            return False

    return True


def display_grid(robots: list[Robot], W=101, H=103) -> None:
    grid = [[0 for _ in range(W)] for _ in range(H)]
    for robot in robots:
        grid[robot.p.y][robot.p.x] += 1
    for r in range(len(grid)):
        for c in range(len(grid[0])):
            print(grid[r][c], end="")
        print("")
    print("\n\n\n")


def parse_input(path: str) -> list[Robot]:
    robots = []
    for l in open(path, "r").readlines():
        m = re.match(r"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)", l)
        assert m
        robots.append(
            Robot(
                Pos(int(m.group(1)), int(m.group(2))),
                Pos(int(m.group(3)), int(m.group(4))),
            )
        )
    return robots


if __name__ == "__main__":
    robots = parse_input(sys.argv[1])
    print(a(deepcopy(robots)))
    print(b(deepcopy(robots)))
