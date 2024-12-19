import sys


def solve_design(towels: list[str], design: str, design_cache={"": 1}) -> int:
    n = 0
    if design in design_cache:
        return design_cache[design]
    for towel in towels:
        if design.startswith(towel):
            n += solve_design(towels, design[len(towel) :])
    design_cache[design] = n
    return n


def a(towels: list[str], designs: list[str]) -> int:
    n = 0

    for design in designs:
        if solve_design(towels, design) > 0:
            n += 1

    return n


def b(towels: list[str], designs: list[str]) -> int:
    total = 0

    for design in designs:
        if n := solve_design(towels, design):
            total += n

    return total


def parse_input(path: str) -> tuple[list[str], list[str]]:
    with open(path, "r") as f:
        towels = f.readline().strip().split(", ")
        f.readline()
        designs = [l.strip() for l in f.readlines()]
    return towels, designs


if __name__ == "__main__":
    towels, designs = parse_input(sys.argv[1])
    print(towels, designs)
    print(a(towels, designs))
    print(b(towels, designs))
