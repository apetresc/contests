import sys


def validate_rules(rules: list[tuple[int, int]], update: list[int]) -> bool:
    for a, b in rules:
        if a in update and b in update and update.index(b) < update.index(a):
            return False
    return True


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
            total += update[len(update) // 2]
    return total


if __name__ == "__main__":
    print(main())
