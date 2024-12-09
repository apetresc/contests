import sys
from itertools import product
from typing import Any


def display_operation(operation, values, evaluate=False):
    s = str(values[0])
    for op, v in zip(operation, values[1:]):
        s = f"{s} {op} {v}"
    if evaluate:
        s = f"{calculate_operation(operation, values)} = {s}"
    print(s)


def calculate_operation(operation, values):
    operators = list(operation)
    args = list(values)
    acc = args.pop(0)
    while len(args) > 0:
        acc = (operators.pop(0))(acc, args.pop(0))
    return acc


class Add:
    def __call__(self, x: int, y: int) -> int:
        return x + y

    def __str__(self):
        return "+"


class Mul:
    def __call__(self, x: int, y: int) -> int:
        return x * y

    def __str__(self):
        return "*"


class Concat:
    def __call__(self, x: int, y: int) -> int:
        return int(str(x) + str(y))

    def __str__(self):
        return "||"


def find_solutions(
    calibrations: list[tuple[int, list[int]]], operations: list[Any]
) -> int:
    total = 0
    for target, values in calibrations:
        for operation in product(operations, repeat=len(values) - 1):
            result = calculate_operation(operation, values)
            if result == target:
                display_operation(operation, values, True)
                total += target
                break
    return total


def a(calibrations: list[tuple[int, list[int]]]) -> int:
    return find_solutions(calibrations, [Add(), Mul()])


def b(calibrations: list[tuple[int, list[int]]]) -> int:
    return find_solutions(calibrations, [Add(), Mul(), Concat()])


def parse_input(path: str) -> list[tuple[int, list[int]]]:
    return [
        (
            int(l.split(":")[0]),
            [int(x) for x in l.split(":")[1].strip().split(" ")],
        )
        for l in open(path, "r").readlines()
    ]


if __name__ == "__main__":
    calibrations: list[tuple[int, list[int]]] = parse_input(sys.argv[1])
    print(calibrations)
    print(a(calibrations))
    print(b(calibrations))
