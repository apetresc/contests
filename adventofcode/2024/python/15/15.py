from __future__ import annotations

import sys
from dataclasses import dataclass
from typing import Generic, Optional, TypeVar

T = TypeVar("T")


@dataclass(frozen=True)
class Pos:
    x: int
    y: int

    def __add__(self, other: Pos | tuple[int, int]) -> Pos:
        if isinstance(other, Pos):
            return Pos(self.x + other.x, self.y + other.y)
        elif isinstance(other, tuple):
            return Pos(self.x + other[0], self.y + other[1])

    def __sub__(self, other: Pos | tuple[int, int]) -> Pos:
        if isinstance(other, Pos):
            return Pos(self.x - other.x, self.y - other.y)
        elif isinstance(other, tuple):
            return Pos(self.x - other[0], self.y - other[1])

    def to(self, other: Pos) -> list[Pos]:
        """
        Returns
        """
        if self.x == other.x:
            return [
                Pos(self.x, y)
                for y in range(self.y, other.y, 1 if self.y <= other.y else -1)
            ]
        elif self.y == other.y:
            return [
                Pos(x, self.y)
                for x in range(self.x, other.x, 1 if self.x <= other.x else -1)
            ]
        else:
            raise ValueError(f"{other} must be orthogonally collinear to {self}")


class Grid(Generic[T]):

    def __init__(self, grid: list[list[T]]):
        self.grid = [[c for c in r] for r in grid]
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

    def show(self, markers: dict[Pos, str]) -> None:
        for y in range(self.h - 1, -1, -1):
            for x in range(self.w):
                pos = Pos(x, y)
                if pos in markers:
                    print(markers[pos], end="")
                else:
                    print(self[pos], end="")
            print("")


def parse_instructions(instructions: list[str]) -> list[Pos]:
    dirs = []
    for instruction in instructions:
        match instruction:
            case ">":
                dirs.append(Pos(1, 0))
            case "<":
                dirs.append(Pos(-1, 0))
            case "^":
                dirs.append(Pos(0, 1))
            case "v":
                dirs.append(Pos(0, -1))
            case _:
                raise ValueError("Invalid instruction: ", instruction)
    return dirs


def a(map: Grid[str], instructions: list[str]) -> int:
    pos = map.find("@")[0]
    map[pos] = "."

    for dir in parse_instructions(instructions):
        npos = pos + dir
        while map.is_in(npos) and map[npos] == "O":
            npos = npos + dir
        if map[npos] == ".":
            # We can move
            map[npos], map[pos + dir] = map[pos + dir], map[npos]
            pos = pos + dir
        else:
            # Blocked, ignore instruction
            pass
        # map.show({pos: "@"})

    # Now we just calculate the total GPS score
    score = 0
    for p in map.find("O"):
        score += p.x + 100 * (map.h - p.y - 1)
    return score


def b(map: Grid[str], instructions: list[str]) -> int:
    pos = map.find("@")[0]
    map[pos] = "."

    def box(grid: Grid[str], pos: Pos) -> Optional[tuple[Pos, Pos]]:
        if grid[pos] == "[":
            return (pos, pos + (1, 0))
        elif grid[pos] == "]":
            return (pos + (-1, 0), pos)
        else:
            return None

    for dir in parse_instructions(instructions):
        npos = pos + dir
        if dir in [Pos(1, 0), Pos(-1, 0)]:  # Horizontal
            # When moving horizontally, boxes can only push one other box
            # so we can treat it almost like part (a)
            while map.is_in(npos) and map[npos] in ["[", "]"]:
                npos = npos + dir
            if map[npos] == ".":
                # We can move
                for p in npos.to(pos):
                    map[p] = map[p - dir]
                pos = pos + dir
        else:
            # Vertical
            boxes_to_move = set()
            frontier = [npos]
            move = True

            while frontier:
                if any([map[p] == "#" for p in frontier]):
                    move = False
                    break
                check_frontier = frontier
                frontier = []
                for p in check_frontier:
                    if b := box(map, p):
                        boxes_to_move.add(b)
                        frontier += [p + dir for p in b]
            if move:
                # Move all boxes by one in direction of dir
                # Therefore, let's sort the boxes according to -dir, so that
                # we move all the furthest boxes first, so we don't need to
                # worry about overlaps.
                for b in sorted(list(boxes_to_move), key=lambda x: x[0].y * -dir.y):
                    map[b[0] + dir], map[b[1] + dir] = map[b[0]], map[b[1]]
                    map[b[0]] = map[b[1]] = "."
                pos = pos + dir
        # map.show({pos: "@"})

    score = 0
    for p in map.find("["):
        score += p.x + 100 * (map.h - p.y - 1)
    return score


def parse_input(path: str) -> tuple[Grid[str], Grid[str], list[str]]:
    def scale(r: str) -> str:
        return (
            r.replace("#", "##")
            .replace("O", "[]")
            .replace(".", "..")
            .replace("@", "@.")
        )

    with open(path, "r") as f:
        grid = []
        scaled_grid = []
        instructions = []
        while (l := f.readline().strip()) != "":
            grid.append([c for c in l])
            scaled_grid.append([c for c in scale(l)])
        while l := f.readline().strip():
            instructions += [c for c in l]
    map = Grid(grid)
    scaled_map = Grid(scaled_grid)

    return map, scaled_map, instructions


if __name__ == "__main__":
    map, scaled_map, instructions = parse_input(sys.argv[1])
    print(a(map, instructions))
    print(b(scaled_map, instructions))
