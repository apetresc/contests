from __future__ import annotations

import heapq
import sys
from copy import deepcopy
from dataclasses import dataclass
from typing import Generic, Optional, TypeVar

T = TypeVar("T")


@dataclass(frozen=True, order=True)
class Pos:
    x: int
    y: int

    def __add__(self, other: Pos | tuple[int, int]) -> Pos:
        if isinstance(other, Pos):
            return Pos(self.x + other.x, self.y + other.y)
        elif isinstance(other, tuple):
            return Pos(self.x + other[0], self.y + other[0])

    def to(self, other: Pos) -> list[Pos]:
        """
        Returns
        """
        if self.x == other.x:
            return [Pos(self.x, y) for y in range(self.y, other.y)]
        elif self.y == other.y:
            return [Pos(x, self.y) for x in range(self.x, other.x)]
        else:
            raise ValueError(f"{other} must be orthogonally colinear to {self}")


class Grid(Generic[T]):

    def __init__(self, grid: list[list[T]]):
        self.grid = grid
        self.h = len(grid)
        self.w = len(grid[0])

    def __check_bounds__(self, pos: Pos) -> None:
        if not (0 <= pos.x < self.w and 0 <= pos.y < self.h):
            raise IndexError("Position {pos} out of bounds", pos)

    def __getitem__(self, pos: Pos) -> T:
        self.__check_bounds__(pos)
        return self.grid[self.h - pos.y - 1][pos.x]

    def __setitem__(self, pos: Pos, val: T) -> None:
        self.__check_bounds__(pos)
        self.grid[self.h - pos.y - 1][pos.x] = val

    def __repr__(self) -> str:
        return f"[{self.w}x{self.h} Grid of {T}]"

    def __str__(self) -> str:
        s = ""
        for r in range(len(self.grid)):
            s += "".join([str(c) for c in self.grid[r]]) + "\n"
        return s

    def find(self, val: T) -> list[Pos]:
        p = []
        for x in range(self.w):
            for y in range(self.h):
                if self[Pos(x, y)] == val:
                    p.append(Pos(x, y))
        return p

    def is_in(self, pos: Pos) -> bool:
        try:
            self.__check_bounds__(pos)
            return True
        except IndexError:
            return False

    def show(self, markers: Optional[dict[Pos, str]] = None) -> None:
        for y in range(self.h - 1, -1, -1):
            for x in range(self.w):
                pos = Pos(x, y)
                if markers and pos in markers:
                    print(markers[pos], end="")
                else:
                    print(self[pos], end="")
            print("")


def bfs(maze: Grid[str], start: Pos, target: Pos) -> int:
    q = [(0, start, Pos(1, 0))]
    seen = set()

    while q:
        score, pos, dir = heapq.heappop(q)
        if pos == target:
            return score
        seen.add((pos, dir))

        # Add the 90-degree rotations to the queue
        for ndir in [Pos(-dir.y, dir.x), Pos(dir.y, -dir.x)]:
            if (pos, ndir) not in seen and maze[pos + ndir] == ".":
                heapq.heappush(q, (score + 1000, pos, ndir))

        # Add straight-ahead movement to the queue
        if maze[pos + dir] == "." and (pos + dir, dir) not in seen:
            heapq.heappush(q, (score + 1, pos + dir, dir))

    return -1


def bfs_path(maze: Grid[str], start: Pos, target: Pos, max_cost: int) -> int:
    q = [(0, start, Pos(1, 0), [])]
    best_spots = set()
    seen = set()

    while q:
        score, pos, dir, path = heapq.heappop(q)
        path = path + [pos]
        if pos == target:
            maze.show({p: "+" for p in path})
            for p in path:
                best_spots.add(p)
        seen.add((pos, dir))

        # Add the 90-degree rotations to the queue
        for ndir in [Pos(-dir.y, dir.x), Pos(dir.y, -dir.x)]:
            if (
                score + 1000 <= max_cost
                and (pos, ndir) not in seen
                and maze[pos + ndir] == "."
            ):
                heapq.heappush(q, (score + 1000, pos, ndir, path))

        # Add straight-ahead movement to the queue
        if (
            score + 1 <= max_cost
            and maze[pos + dir] == "."
            and (pos + dir, dir) not in seen
        ):
            heapq.heappush(q, (score + 1, pos + dir, dir, path))

    maze.show({p: "O" for p in best_spots})
    return len(best_spots)


def a(maze: Grid[str]) -> int:
    pos = maze.find("S")[0]
    target = maze.find("E")[0]
    maze[pos] = "."
    maze[target] = "."

    return bfs(maze, pos, target)


def b(maze: Grid[str], max_cost: int) -> int:
    pos = maze.find("S")[0]
    target = maze.find("E")[0]
    maze[pos] = "."
    maze[target] = "."

    return bfs_path(maze, pos, target, max_cost)


def parse_input(path: str) -> Grid[str]:
    grid = []
    with open(path, "r") as f:
        while (l := f.readline().strip()) != "":
            grid.append([c for c in l])
    return Grid(grid)


if __name__ == "__main__":
    maze = parse_input(sys.argv[1])
    max_cost = a(deepcopy(maze))
    print(max_cost)
    print(b(maze, max_cost))
