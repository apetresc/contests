import sys

terrain = [line.strip() for line in sys.stdin]
total = 1

for dx, dy in [(1,1), (3,1), (5, 1), (7, 1), (1, 2)]:
    trees = 0
    x = 0
    for y in range(dy, len(terrain), dy):
        x += dx
        if terrain[y][x % len(terrain[y])] == '#':
            trees += 1
    print(trees)
    total *= trees

print(total)