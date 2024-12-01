import sys


def main():
    a, b = [], []
    for l in open(sys.argv[1], 'r').readlines():
        x, y = l.split()
        a.append(int(x))
        b.append(int(y))


    return sum(map(lambda x: x * b.count(x), a))


if __name__ == "__main__":
    print(main())
