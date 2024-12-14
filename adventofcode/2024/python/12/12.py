import sys


def area(grid: list[list[str]], region: set[tuple[int, int]]) -> int:
    return len(region)


def perimeter(region_map: list[list[int]], region: set[tuple[int, int]]) -> int:
    p = 0
    for plot in region:
        p += 4
        for dx, dy in [(-1, 0), (0, -1), (1, 0), (0, 1)]:
            (px, py) = (plot[0] + dx, plot[1] + dy)
            if (
                0 <= px < len(region_map)
                and 0 <= py < len(region_map[0])
                and (px, py) in region
            ):
                p -= 1
    return p


def vertices(
    grid: list[list[str]], region_map: list[list[int]], region: set[tuple[int, int]]
) -> int:
    """
    Counts the number of vertices (i.e, "corners") in the shape of the given
    region. By Euler's formula, this is equal to the number of edges in the
    shape.

    It does this by looking at each of the four possible corner locations and
    comparing against a specific mask:

    B X       In this "convex" variant, we look for the orthogonally adjacent
    -+   <--  cells to be from a different region. This guarantees a convex
    A|B       corner.


    A|B      In this "concave" variant, we look for the orthogonally adjacent
     +-  <-- cells to be from the same region, but the diagonally opposite cell
    A A      to be from a different region. This guarantees a concave corner.
    """
    v = 0
    for plot in region:
        for (e1x, e1y), (e2x, e2y) in [
            ((1, 0), (0, -1)),
            ((-1, 0), (0, 1)),
            ((0, 1), (1, 0)),
            ((0, -1), (-1, 0)),
        ]:
            e1 = (plot[0] + e1x, plot[1] + e1y)
            e2 = (plot[0] + e2x, plot[1] + e2y)
            ed = (plot[0] + e1x + e2x, plot[1] + e1y + e2y)
            e1joined = (
                (0 <= e1[0] < len(grid))
                and (0 <= e1[1] < len(grid[0]))
                and e1 in region
            )
            e2joined = (
                (0 <= e2[0] < len(grid))
                and (0 <= e2[1] < len(grid[0]))
                and e2 in region
            )
            if (not e1joined and not e2joined) or (
                e1joined
                and e2joined
                and region_map[ed[0]][ed[1]] != region_map[plot[0]][plot[1]]
            ):
                v += 1

    return v


def label(grid: list[list[str]], region: set[tuple[int, int]]) -> str:
    pos = next(iter(region))
    return grid[pos[0]][pos[1]]


def flood_fill(grid: list[list[str]], start: tuple[int, int]) -> set[tuple[int, int]]:
    region = set()
    q = [start]

    while q:
        pos = q.pop()
        region.add(pos)
        for dx, dy in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            npos = (pos[0] + dx, pos[1] + dy)
            if (
                0 <= npos[0] < len(grid)
                and 0 <= npos[1] < len(grid[0])
                and grid[npos[0]][npos[1]] == grid[start[0]][start[1]]
                and npos not in region
            ):
                q.append(npos)

    return region


def get_regions(
    grid: list[list[str]],
) -> tuple[list[list[int]], list[set[tuple[int, int]]]]:
    region_map = [[0 for _ in range(len(grid[0]))] for _ in range(len(grid))]
    regions: list[set[tuple[int, int]]] = []
    for r in range(len(grid)):
        for c in range(len(grid[0])):
            if region_map[r][c] == 0:
                # We don't have a region for this cell yet
                region = flood_fill(grid, (r, c))
                regions.append(region)
                region_id = len(regions)
                for plot in region:
                    region_map[plot[0]][plot[1]] = region_id
    return region_map, regions


def a(grid: list[list[str]]) -> int:
    region_map, regions = get_regions(grid)
    total = 0
    for region in regions:
        print(
            f"Region {label(grid, region)} has area {area(grid, region)} and perimeter {perimeter(region_map, region)}: {region}"
        )
        total += area(grid, region) * perimeter(region_map, region)
    return total


def b(grid: list[list[str]]) -> int:
    region_map, regions = get_regions(grid)
    total = 0
    for region in regions:
        print(
            f"Region {label(grid, region)} has area {area(grid, region)} and edges {vertices(grid, region_map, region)}: {region}"
        )
        total += area(grid, region) * vertices(grid, region_map, region)
    return total


def parse_input(path: str) -> list[list[str]]:
    with open(path, "r") as f:
        grid = [[c for c in line.strip()] for line in f.readlines()]
    return grid


if __name__ == "__main__":
    grid = parse_input(sys.argv[1])
    print(a(grid))
    print(b(grid))
