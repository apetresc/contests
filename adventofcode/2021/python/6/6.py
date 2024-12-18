import sys


def a(fishes: list[int], days: int) -> int:
    for _ in range(days):
        new_fish = []
        for fish in fishes:
            if fish > 0:
                new_fish.append(fish - 1)
            else:
                new_fish.append(8)
                new_fish.append(6)
        fishes = new_fish
    return len(fishes)


def b(fishes: list[int], days: int) -> int:
    cohorts = [0 for _ in range(9)]
    for fish in fishes:
        cohorts[fish] += 1

    for _ in range(days):
        new_parents = cohorts[0]
        cohorts = cohorts[1:] + [new_parents]
        cohorts[6] += new_parents

    return sum(cohorts)


def parse_input(path: str) -> list[int]:
    return list(map(int, open(path, "r").readline().strip().split(",")))


if __name__ == "__main__":
    fishes = parse_input(sys.argv[1])
    print(a(fishes, 80))
    print(b(fishes, 256))
