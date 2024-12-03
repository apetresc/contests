import re
import sys


def parse_segment(segment: str) -> int:
    matches = re.findall(r"mul\((\d+),(\d+)\)", segment)
    return sum([int(x) * int(y) for x, y in matches])


def main():
    program = open(sys.argv[1], "r").read().strip()
    dont_blocks = re.split(r"don't\(\)", program)
    total = parse_segment(dont_blocks[0])
    for dont_block in dont_blocks[1:]:
        for do_block in re.split(r"do\(\)", dont_block)[1:]:
            total += parse_segment(do_block)
    return total


if __name__ == "__main__":
    print(main())
