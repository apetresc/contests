import sys


def main():
    a, b = [], []
    for l in open(sys.argv[1], 'r').readlines():
        x, y = l.split()
        a.append(int(x))
        b.append(int(y))

    return sum(map(lambda x: abs(x[0] - x[1]), zip(sorted(a), sorted(b))))


if __name__ == "__main__":
    print(main())
