import sys

position = [0, 0]
direction = [1, 0]
rots = [[1, 0], [0, 1], [-1, 0], [0, -1]] # left rotations

for l in sys.stdin:
    a = l[0]
    d = int(l[1:].strip())

    if a == 'N':
        position[1] += d
    elif a == 'S':
        position[1] -= d
    elif a == 'E':
        position[0] += d
    elif a == 'W':
        position[0] -= d
    elif a == 'F':
        position[0] += direction[0] * d
        position[1] += direction[1] * d
    elif a == 'L':
        direction = rots[(rots.index(direction) + (d // 90)) % 4]
    elif a == 'R':
        direction = rots[::-1][(rots[::-1].index(direction) + (d // 90)) % 4]
    print(f"Now at {position} facing {direction} after {a} {d}")

print(position, direction)