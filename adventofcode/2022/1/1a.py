import sys


print(
        max(
            map(lambda x: sum(map(int, x.strip().split("\n"))),
                "".join(sys.stdin.readlines()).split("\n\n"))))
