from __future__ import annotations

import heapq
import sys
from collections import defaultdict
from dataclasses import dataclass
from itertools import product
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


def dijkstra(track: Grid[str], source: Pos) -> Grid[int]:
    distances = Grid(w=track.w, h=track.h, default_value=track.w * track.h)
    distances[source] = 0
    q = [(0.0, source)]

    while q:
        _, pos = heapq.heappop(q)
        for dir in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            npos = pos + dir
            if track.is_in(npos) and track[npos] == ".":
                alt = distances[pos] + 1
                if alt < distances[npos]:
                    distances[npos] = alt
                    heapq.heappush(q, (alt, npos))

    return distances


def d(p1: Pos, p2: Pos) -> int:
    """Returns the Manhattan distance between p1 and p2"""
    return abs(p1.x - p2.x) + abs(p1.y - p2.y)


def find_cheats(track: Grid[str], start: Pos, end: Pos, cheat_length: int) -> int:
    start_distances = dijkstra(track, start)
    end_distances = dijkstra(track, end)
    print("Calculated distances")
    cheats: dict[int, set[tuple[Pos, Pos]]] = defaultdict(set)

    for cheat_start in track.find("."):
        if start_distances[cheat_start] > start_distances[end] - 100:
            continue
        for dir in product(range(-cheat_length, cheat_length + 1), repeat=2):
            cheat_end = cheat_start + (dir[0], dir[1])
            if (
                d(cheat_start, cheat_end) <= cheat_length
                and track.is_in(cheat_end)
                and track[cheat_end] == "."
                and cheat_start != cheat_end
            ):
                cheat_distance = (
                    start_distances[cheat_start]
                    + d(cheat_start, cheat_end)
                    + end_distances[cheat_end]
                )
                if cheat_distance < start_distances[end]:
                    cheats[start_distances[end] - cheat_distance].add(
                        (cheat_start, cheat_end)
                    )

    return sum(len(v) for (k, v) in cheats.items() if k >= 100)


def a(track: Grid[str], start: Pos, end: Pos) -> int:
    return find_cheats(track, start, end, 2)


def b(track: Grid[str], start: Pos, end: Pos) -> int:
    return find_cheats(track, start, end, 20)


def parse_input(path: str) -> Grid[str]:
    track = []
    with open(path, "r") as f:
        while (l := f.readline().strip()) != "":
            track.append([c for c in l])
    return Grid(track)


if __name__ == "__main__":
    track = parse_input(sys.argv[1])
    start = track.find("S")[0]
    end = track.find("E")[0]
    track[start] = "."
    track[end] = "."
    track.show()
    print(a(track, start, end))
    print(b(track, start, end))
