import re
import sys
from typing import Literal, NamedTuple

import numpy as np


class Pos(NamedTuple):
    x: int
    y: int


class Machine(NamedTuple):
    a: Pos
    b: Pos
    prize: Pos


def solve(machine: Machine) -> tuple[int, int] | Literal[False]:
    a = np.array([[machine.a.x, machine.b.x], [machine.a.y, machine.b.y]])
    b = np.array([machine.prize.x, machine.prize.y])
    x = np.linalg.solve(a, b).round().astype(int)
    if (x[0] * machine.a.x + x[1] * machine.b.x == machine.prize.x) and (
        x[0] * machine.a.y + x[1] * machine.b.y == machine.prize.y
    ):
        print(f"Found solution for {machine}: {[x[0], x[1]]}")
        return tuple(x)
    else:
        return False


def a(machines: list[Machine]) -> int:
    tokens = 0
    for machine in machines:
        solution = solve(machine)
        if solution:
            tokens += 3 * solution[0] + solution[1]
    return tokens


def b(machines: list[Machine]) -> int:
    OFFSET = 10000000000000
    tokens = 0
    for machine in machines:
        fixed_machine = Machine(
            machine.a,
            machine.b,
            Pos(machine.prize.x + OFFSET, machine.prize.y + OFFSET),
        )
        solution = solve(fixed_machine)
        if solution:
            tokens += 3 * solution[0] + solution[1]
    return tokens


def parse_input(path: str) -> list[Machine]:
    machines = []
    with open(path, "r") as f:
        while l1 := f.readline():
            l2 = f.readline()
            l3 = f.readline()
            a = re.match(r"Button A: X\+(\d+), Y\+(\d+)", l1)
            b = re.match(r"Button B: X\+(\d+), Y\+(\d+)", l2)
            prize = re.match(r"Prize: X=(\d+), Y=(\d+)", l3)
            assert a and b and prize
            machines.append(
                Machine(
                    a=Pos(int(a.group(1)), int(a.group(2))),
                    b=Pos(int(b.group(1)), int(b.group(2))),
                    prize=Pos(int(prize.group(1)), int(prize.group(2))),
                )
            )
            f.readline()

    return machines


if __name__ == "__main__":
    machines = parse_input(sys.argv[1])
    print(a(machines))
    print(b(machines))
