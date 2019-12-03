def tadd(t1, t2):
    return (t1[0] + t2[0], t1[1] + t2[1])

path1 = input().split(",")
path2 = input().split(",")

pos1 = [(0,0)]
for m in path1:
    for _ in range(int(m[1:])):
        if m[0] == 'R':
            pos1.append(tadd(pos1[-1], (0, 1)))
        if m[0] == 'D':
            pos1.append(tadd(pos1[-1], (-1, 0)))
        if m[0] == 'L':
            pos1.append(tadd(pos1[-1], (0, -1)))
        if m[0] == 'U':
            pos1.append(tadd(pos1[-1], (1, 0)))
pos1 = pos1[1:]


pos2 = [(0,0)]
for m in path2:
    for _ in range(int(m[1:])):
        if m[0] == 'R':
            pos2.append(tadd(pos2[-1], (0, 1)))
        if m[0] == 'D':
            pos2.append(tadd(pos2[-1], (-1, 0)))
        if m[0] == 'L':
            pos2.append(tadd(pos2[-1], (0, -1)))
        if m[0] == 'U':
            pos2.append(tadd(pos2[-1], (1, 0)))
pos2 = pos2[1:]

print("pos1:", pos1)
print("pos2:", pos2)

intersections = set(pos1).intersection(set(pos2))
print("intersections:", intersections)
min_distance = 99999999999999999
for intersection in intersections:
    min_distance = min(min_distance, abs(intersection[0]) + abs(intersection[1]))

print(min_distance)
