import sys


cycle, X = 1, 1
screen = [["." for _ in range(40)] for _ in range(6)]

def tick():
    global cycle
    x, y = (cycle - 1) % 40, ((cycle - 1) // 40) % 6
    screen[y][x] = '#' if abs(x - X) <= 1 else '.'
    cycle += 1


for l in sys.stdin.readlines():
    if l.startswith("noop"):
        tick()
    elif l.startswith("addx"):
        tick()
        tick()
        X += int(l.split()[1])

print('\n'.join([''.join(r) for r in screen]))
