import re
import sys


def main():
    program = open(sys.argv[1], "r").read().strip()
    matches = re.findall(r"mul\((\d+),(\d+)\)", program)
    return sum([int(x) * int(y) for x, y in matches])


if __name__ == "__main__":
    print(main())
