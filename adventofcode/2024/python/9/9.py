import sys


def expand(n: str) -> list[int]:
    blocks = []
    i = 0
    k = 0
    while n:
        d = int(n[0])
        n = n[1:]
        if i % 2 == 0:
            blocks += [k] * d
            k += 1
        else:
            blocks += [-1] * d
        i += 1
    return blocks


def defrag_blocks(blocks: list[int]) -> None:
    head = 0
    tail = len(blocks) - 1

    while True:
        # Advance head to first empty block
        while head < len(blocks) and blocks[head] > -1:
            head += 1
        if head >= len(blocks):
            break
        # Advance tail to first non-empty block
        while blocks[tail] == -1:
            tail -= 1
        if head > tail or tail <= 0:
            break
        blocks[head] = blocks[tail]
        blocks[tail] = -1


def defrag_files(blocks: list[int]) -> None:
    tail = len(blocks) - 1
    while True:
        while blocks[tail] == -1:
            tail -= 1
        if tail < 0:
            break
        # tail is now pointing to the LAST block of a file
        file_size = 1
        while (tail - file_size > 0) and blocks[tail - file_size] == blocks[tail]:
            file_size += 1
        assert blocks[tail - file_size + 1 : tail + 1] == [blocks[tail]] * file_size
        # Now we need to find the first contiguous block of space that fits file_size
        # TODO: this should definitely be cached/indexed, I feel terrible about this lol
        head = 0
        while head < (tail - file_size + 1) and head < len(blocks) - file_size:
            if blocks[head : head + file_size] == [-1] * file_size:
                # We found a spot! Do the swap
                blocks[head : head + file_size] = blocks[
                    tail - file_size + 1 : tail + 1
                ]
                blocks[tail - file_size + 1 : tail + 1] = [-1] * file_size
                break
            head += 1
        tail -= file_size


def checksum(blocks: list[int]) -> int:
    return sum(map(lambda x: x[0] * max(0, x[1]), enumerate(blocks)))


def display_blocks(blocks: list[int]):
    print("".join(["." if x < 0 else (str(x) if x <= 9 else f"({x})") for x in blocks]))


def a(n: str) -> int:
    blocks = expand(n)
    defrag_blocks(blocks)
    display_blocks(blocks)
    return checksum(blocks)


def b(n: str) -> int:
    blocks = expand(n)
    defrag_files(blocks)
    return checksum(blocks)


def parse_input(path: str) -> str:
    return open(path, "r").read().strip()


if __name__ == "__main__":
    n = parse_input(sys.argv[1])
    print(a(n))
    print(b(n))
