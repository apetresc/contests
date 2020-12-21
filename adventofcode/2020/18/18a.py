import sys
from typing import List


OPS = {
    '+': lambda x, y: x + y,
    '*': lambda x, y: x * y
}

def tokenize(l : str):
    return l.replace('(', '( ').replace(')', ' )').split()

def parse_expr(e : List[str]) -> int:
    assert e[0] == '('
    e.pop(0)

    acc = []
    op = None
    while e[0] != ')':
        if e[0] == '(':
            acc.append(parse_expr(e))
        elif e[0] in ['*', '+']:
            op = e.pop(0)
            continue
        else:
            acc.append(int(e.pop(0)))
        if len(acc) == 2:
            acc.append(OPS[op](acc.pop(), acc.pop()))
    assert e[0] == ')'
    e.pop(0)
    return acc.pop()

print(sum(map(lambda l: parse_expr(tokenize('(' + l.strip() + ')')), sys.stdin.readlines())))