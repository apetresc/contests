from collections import defaultdict
import sys

GENERATION = 1


def mmz(board, padding=0):
    #return range(min([k for k in board.keys() if len(board[k]) > 0]) - padding, max([k for k in board.keys() if len(board[k]) > 0]) + padding + 1)
    return range(-GENERATION - padding, GENERATION + padding + 1)

def mmy(board, padding=0):
    #return range(*(lambda v: (min(v) - padding, max(v) + padding + 1))(sum([list(x.keys()) for x in list(board.values())], [])))
    return range(-GENERATION - 8 - padding, GENERATION + 8 + padding + 1)

def mmx(board, padding=0):
    #return range(*(lambda v: (min(v) - padding , max(v) + padding + 1))(sum([sum([list(board[x][y].keys()) for y in board[x].keys()], []) for x in board.keys()], [])))
    return range(-GENERATION - 8 - padding, GENERATION + 8 + padding + 1)

def mmw(board, padding=0):
    return range(-GENERATION - padding, GENERATION + padding + 1)

def pprint(board):
    for w in mmw(board):
        for z in mmz(board):
            print(f"z={z}, w={w}")
            for y in mmy(board):
                for x in mmx(board):
                    print(board[w][z][y][x], end='')
                print()

def num_activated(board):
    return sum([sum([sum([sum([w for w in board[x][y][z].values()]) for z in board[x][y].keys()]) for y in board[x].keys()]) for x in board.keys()])

def adjacent_occupied(board, x, y, z, w):
    adj = 0
    for dx in range(-1, 2):
        for dy in range(-1, 2):
            for dz in range(-1, 2):
                for dw in range(-1, 2):
                    if dx == dy == dz == dw == 0:
                        continue
                    adj += board[w + dw][z + dz][y + dy][x + dx]
    return adj

def evolve(board):
    global GENERATION
    GENERATION += 1
    bp = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: defaultdict(int))))
    ws = mmw(board, padding=1)
    zs = mmz(board, padding=1)
    ys = mmy(board, padding=1)
    xs = mmx(board, padding=1)
    for w in ws:
        for z in zs:
            for y in ys:
                for x in xs:
                    if board[w][z][y][x] == 1:
                        if 2 <= adjacent_occupied(board, x, y, z, w) <= 3:
                            bp[w][z][y][x] = 1
                        else:
                            bp[w][z][y][x] = 0
                    elif board[w][z][y][x] == 0:
                        if adjacent_occupied(board, x, y, z, w) == 3:
                            bp[w][z][y][x] = 1
                        else:
                            bp[w][z][y][x] = 0
    return bp

board = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: defaultdict(int))))

for x, l in enumerate(sys.stdin.readlines()):
    for y, c in enumerate(l.strip()):
        if c == '#':
            board[0][0][x][y] = 1
        else:
            board[0][0][x][y] = 0

for i in range(6):
    board = evolve(board)
    print(f"Done generation {i+1} ({num_activated(board)} activated)")
    pprint(board)