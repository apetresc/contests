import sys

terrain = [line.strip() for line in sys.stdin]
trees = 0
for y in range(1, len(terrain)):
    x = 3 * y
    if terrain[y][x % len(terrain[y])] == '#':
        trees += 1

print(trees)