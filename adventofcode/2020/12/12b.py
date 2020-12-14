import sys

position = [0, 0]
waypoint = [10, 1]
rot = [[0, -1], [1, 0]] # left rotation

for l in sys.stdin:
    a = l[0]
    d = int(l[1:].strip())

    if a == 'N':
        waypoint[1] += d
    elif a == 'S':
        waypoint[1] -= d
    elif a == 'E':
        waypoint[0] += d
    elif a == 'W':
        waypoint[0] -= d
    elif a == 'F':
        position[0] += waypoint[0] * d
        position[1] += waypoint[1] * d
    elif a == 'L':
        while d > 0:
            waypoint = [rot[0][0] * waypoint[0] + rot[0][1] * waypoint[1],
                        rot[1][0] * waypoint[0] + rot[1][1] * waypoint[1]]
            d -= 90
    elif a == 'R':
        while d > 0:
            waypoint = [-rot[0][0] * waypoint[0] - rot[0][1] * waypoint[1],
                        -rot[1][0] * waypoint[0] - rot[1][1] * waypoint[1]]
            d -= 90
    print(f"Now at {position} waypoint {waypoint} after {a} {d}")

print(position)