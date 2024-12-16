from __future__ import annotations

from dataclasses import dataclass
from typing import Generic, TypeVar

T = TypeVar("T")


@dataclass(frozen=True)
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

    def flood_fill(self, start: Pos, vals: list[T]) -> set[Pos]:
        region = set()
        q = []
        if self[start] in vals:
            q.append(start)

        while q:
            pos = q.pop()
            region.add(pos)
            for dir in [(1, 0), (0, 1), (-1, 0), (0, -1)]:
                npos = pos + dir
                if self.is_in(npos) and self[npos] in vals:
                    q.append(npos)

        return region
