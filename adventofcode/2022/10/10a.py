import sys

state = [(1, 1)]

def at(c):
    return [(cycle, X) for cycle, X in state if cycle <= c]

for l in map(lambda l: l.strip(), sys.stdin.readlines()):
    print(l)
    cycle, X = state[-1]
    if l == "noop":
        state.append((cycle + 1, X))
    elif l.startswith("addx"):
        state.append((cycle + 2, X + int(l.split()[1])))

print(state)
print(sum(x * at(x)[-1][1] for x in [20, 60, 100, 140, 180, 220]))