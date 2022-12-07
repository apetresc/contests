import functools
import sys
from pathlib import Path

fs = {"/": {}}
cwd = Path("/")

def ls(p: Path):
    return functools.reduce(lambda a, b: a[b], p.parts, fs)

def du(p: Path):
    if isinstance(ls(p), int):
        return ls(p)
    return sum(map(du, [p / e for e in ls(p)]))

def scan(p: Path):
    yield p
    for d in ls(p):
        if isinstance(ls(p / d), dict):
            yield from scan(p / d)


l = sys.stdin.readline().strip()
while l:
    if l.startswith("$ cd"):
        cwd = cwd.parent if l.split()[2] == '..' else cwd / l.split()[2]
        l = sys.stdin.readline().strip()
    elif l == "$ ls":
        l = sys.stdin.readline().strip()
        while l and not l.startswith("$"):
            if l.startswith("dir"):
                ls(cwd)[l.split()[1]] = {}
            else:
                ls(cwd)[l.split()[1]] = int(l.split()[0])
            l = sys.stdin.readline().strip()
