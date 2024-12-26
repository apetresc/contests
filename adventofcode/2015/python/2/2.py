import sys


def a(packages: list[tuple[int, int ,int]]) -> int:
    total = 0
    for package in packages:
        d1, d2, d3 = sorted(package)
        total += 2 * (d1 * d2 + d2 * d3 + d3 * d1) + d1 * d2
    return total

def b(packages: list[tuple[int, int, int]]) -> int:
    total = 0
    for package in packages:
        d1, d2, d3 = sorted(package)
        total += 2 * (d1 + d2) + d1 * d2 * d3
    return total

def parse_input(path: str) -> list[tuple[int, int , int]]:
    presents = []
    with open(path, "r") as f:
        while (l := f.readline().strip()) != "":
            l, w, h = l.split("x")
            presents.append((int(l), int(w), int(h)))
    return presents

if __name__ == "__main__":
    packages = parse_input(sys.argv[1])
    print(a(packages))
    print(b(packages))
