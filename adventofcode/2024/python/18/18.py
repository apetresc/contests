from __future__ import annotations

import heapq
import sys
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
            return Pos(self.x + other[0], self.y + other[1])

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

    def show(self, markers: Optional[dict[Pos, str]] = None) -> None:
        for y in range(self.h - 1, -1, -1):
            for x in range(self.w):
                pos = Pos(x, y)
                if markers and pos in markers:
                    print(markers[pos], end="")
                else:
                    print(self[pos], end="")
            print("")

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


def bfs(grid: Grid[str], start: Pos, end: Pos) -> Optional[int]:
    q = [(0, start)]
    seen = set()

    while q:
        d, pos = heapq.heappop(q)
        if pos == end:
            return d
        if pos in seen:
            continue
        else:
            seen.add(pos)

        for dir in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            if (
                grid.is_in(pos + dir)
                and grid[pos + dir] == "."
                and (pos + dir) not in seen
            ):
                heapq.heappush(q, (d + 1, (pos + dir)))

    return None


def a(bytes: list[Pos], n=1024, W=71, H=71) -> Optional[int]:
    grid = Grid([["." for _ in range(W)] for _ in range(H)])

    for p in bytes[0:n]:
        grid[p] = "#"

    start = Pos(0, 0)
    end = Pos(W - 1, H - 1)

    return bfs(grid, start, end)


def b(bytes: list[Pos], W=71, H=71) -> Pos:
    grid = Grid([["." for _ in range(W)] for _ in range(H)])
    start = Pos(0, 0)
    end = Pos(W - 1, H - 1)

    for byte in bytes:
        grid[byte] = "#"
        if not bfs(grid, start, end):
            return byte

    return Pos(0, 0)


def parse_input(path: str) -> list[Pos]:
    bytes = []
    with open(path, "r") as f:
        while (l := f.readline().strip().split(",")) != [""]:
            bytes.append(Pos(int(l[0]), int(l[1])))
    return bytes


if __name__ == "__main__":
    bytes = parse_input(sys.argv[1])
    print(a(bytes))
    print(b(bytes))
