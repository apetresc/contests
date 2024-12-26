import sys


def parse_input(path: str) -> str:
    return open(path, "r").readline().strip()


def a(instructions: str) -> int:
    return instructions.count('(') - instructions.count(')')

def b(instructions: str) -> int:
    floor = 0
    for i, c in enumerate(instructions):
        floor += 1 if c == '(' else -1
        if floor < 0:
            return i + 1
    return -1

if __name__ == "__main__":
    instructions = parse_input(sys.argv[1])
    print(a(instructions))
    print(b(instructions))
