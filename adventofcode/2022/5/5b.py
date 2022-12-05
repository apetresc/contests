import sys

drawing, moves = map(lambda ls: ls.split("\n"), "".join(sys.stdin.readlines()).split("\n\n"))
N = max(map(int, drawing[-1].split()))
stacks = [[drawing[i][1 + 4 * j] for i in range(N) if drawing[i][1 + 4 * j] != " "][::-1] for j in range(N)]

for m in moves:
    q, f, t = map(int, m.split()[1:6:2])
    stacks[f-1], c = stacks[f-1][0:-q], stacks[f-1][-q:]
    stacks[t-1].extend(c)

print(''.join([s[-1] for s in stacks if s]))
