from __future__ import annotations

from dataclasses import dataclass
from typing import Generic, Iterable, Optional, TypeVar, cast, overload

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

    def __init__(
        self,
        grid: Optional[Iterable[Iterable[T]]] = None,
        h: Optional[int] = None,
        w: Optional[int] = None,
        default_value: Optional[T] = None,
    ):
        if grid:
            self.grid = [list(row) for row in grid]
            self.h = len(self.grid)
            self.w = len(self.grid[0])
        elif h and w and default_value:
            self.h = h
            self.w = w
            self.grid = [[default_value for _ in range(w)] for _ in range(h)]

    def __check_bounds__(self, pos: Pos) -> None:
        if not (0 <= pos.x < self.w and 0 <= pos.y < self.h):
            raise IndexError("Position {pos} out of bounds", pos)

    @overload
    def __getitem__(self, key: Pos) -> T: ...
    @overload
    def __getitem__(self, key: tuple[slice | int, slice | int]) -> Grid[T]: ...

    def __getitem__(self, key: Pos | tuple[slice | int, slice | int]) -> T | Grid[T]:
        if isinstance(key, Pos):
            pos = cast(Pos, key)
            self.__check_bounds__(pos)
            return self.grid[self.h - pos.y - 1][pos.x]
        elif (
            isinstance(key, tuple)
            and isinstance(key[0], slice | int)
            and isinstance(key[1], slice | int)
        ):
            hs = key[0]
            if isinstance(hs, int):
                hs = slice(hs, hs + 1, 1)
            if isinstance(key[1], int):
                vs = slice(key[1], key[1] + 1, 1)
            else:
                vs = slice(key[1].start or 0, key[1].stop or self.h - 1, key[1].step)

            return Grid(
                row[hs.start : hs.stop : hs.step]
                for row in self.grid[self.h - vs.stop : self.h - vs.start : vs.step]
            )
        raise TypeError("Unexpected slice type", type(slice))

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


# TESTS


def test_grid_slicing():
    grid = Grid(
        map(lambda x: chr(ord("a") + x), range(i, i + 10)) for i in range(5, 15)
    )
    grid.show()
    print("========")
    grid[1:3, 2:5].show()
    print("========")
    grid[3, 0:2].show()
    print("========")
    grid[-1:-4:-1, 2].show()
    print("========")
    grid[1:, 1:].show()
