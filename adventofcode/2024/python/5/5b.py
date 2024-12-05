import sys
from functools import cmp_to_key


def validate_rules(rules: list[tuple[int, int]], update: list[int]) -> list[int]:
    bad_indices = []
    for a, b in rules:
        if a in update and b in update and update.index(b) < update.index(a):
            bad_indices.append((update.index(a), update.index(b)))
    return bad_indices


def fix_update(rules: list[tuple[int, int]], update: list[int]) -> list[int]:
    def cmp_rules(a: int, b: int):
        if (a, b) in rules:
            return -1
        elif (b, a) in rules:
            return 1
        elif a == b:
            return 0
        else:
            return a - b

    return sorted(update, key=cmp_to_key(cmp_rules))


def main():
    lines = open(sys.argv[1], "r").readlines()
    rules: list[tuple[int, int]] = [
        (lambda x: (int(x[0]), int(x[1])))(rule.strip().split("|"))
        for rule in lines[0 : lines.index("\n")]
    ]
    updates = [
        list(map(int, update.strip().split(",")))
        for update in lines[lines.index("\n") + 1 :]
    ]
    total = 0
    for update in updates:
        if validate_rules(rules, update):
            fixed_update = fix_update(rules, update)
            total += fixed_update[len(fixed_update) // 2]
    return total


if __name__ == "__main__":
    print(main())
