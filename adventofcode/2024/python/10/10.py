import sys


def bfs(grid: list[list[int]], start: tuple[int, int]) -> int:
    seen = set()
    score = 0
    q = [start]
    while q:
        pos = q.pop(0)
        if pos in seen:
            continue
        else:
            seen.add(pos)
        if grid[pos[0]][pos[1]] == 9:
            score += 1
        # Add neighbours with gradient is exactly +1
        for dx, dy in [(1,0), (-1,0), (0,1), (0,-1)]:
            npos = (pos[0] + dx, pos[1] + dy)
            if 0 <= npos[0] < len(grid) and 0 <= npos[1] < len(grid[0]) \
                and grid[npos[0]][npos[1]] - grid[pos[0]][pos[1]] == 1 \
                and npos not in seen:
                q.append(npos)
    return score


def bfs_count_paths(grid: list[list[int]], start: tuple[int, int]) -> int:
    score = 0
    q = [start]
    while q:
        pos = q.pop(0)
        if grid[pos[0]][pos[1]] == 9:
            score += 1
        # Add neighbours with gradient is exactly +1
        for dx, dy in [(1,0), (-1,0), (0,1), (0,-1)]:
            npos = (pos[0] + dx, pos[1] + dy)
            if 0 <= npos[0] < len(grid) and 0 <= npos[1] < len(grid[0]) \
                and grid[npos[0]][npos[1]] - grid[pos[0]][pos[1]] == 1:
                q.append(npos)
    return score


def find_trailheads(grid: list[list[int]]) -> list[tuple[int, int]]:
    trailheads = []
    for r in range(len(grid)):
        for c in range(len(grid[0])):
            if grid[r][c] == 0:
                trailheads.append((r, c))
    return trailheads

def a(grid: list[list[int]]) -> int:
    trailheads = find_trailheads(grid)

    total = 0
    for trailhead in trailheads:
        score = bfs(grid, trailhead)
        total += score
    return total

def b(grid: list[list[int]]) -> int:
    trailheads = find_trailheads(grid)

    total = 0
    for trailhead in trailheads:
        score = bfs_count_paths(grid, trailhead)
        total += score
    return total


def parse_input(path: str) -> list[list[int]]:
    with open(path, "r") as f:
        grid = [[int(c) for c in line.strip()] for line in f.readlines()]
    return grid


if __name__ == "__main__":
    grid = parse_input(sys.argv[1])
    print(a(grid))
    print(b(grid))
