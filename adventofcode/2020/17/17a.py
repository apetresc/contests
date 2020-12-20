from collections import defaultdict
import sys


def mmz(board, padding=0):
    return range(min([k for k in board.keys() if len(board[k]) > 0]) - padding, max([k for k in board.keys() if len(board[k]) > 0]) + padding + 1)

def mmy(board, padding=0):
    return range(*(lambda v: (min(v) - padding, max(v) + padding + 1))(sum([list(x.keys()) for x in list(board.values())], [])))

def mmx(board, padding=0):
    return range(*(lambda v: (min(v) - padding , max(v) + padding + 1))(sum([sum([list(board[x][y].keys()) for y in board[x].keys()], []) for x in board.keys()], [])))

def pprint(board):
    for z in mmz(board):
        print(f"z={z}")
        for y in mmy(board):
            for x in mmx(board):
                print(board[z][y][x], end='')
            print()

def num_activated(board):
    return sum([sum([sum([z for z in board[x][y].values()]) for y in board[x].keys()]) for x in board.keys()])

def adjacent_occupied(board, x, y, z):
    adj = 0
    for dx in range(-1, 2):
        for dy in range(-1, 2):
            for dz in range(-1, 2):
                if dx == dy == dz == 0:
                    continue
                adj += board[z + dz][y + dy][x + dx]
    return adj

def evolve(board):
    bp = defaultdict(lambda: defaultdict(lambda: defaultdict(int)))
    zs = mmz(board, padding=1)
    ys = mmy(board, padding=1)
    xs = mmx(board, padding=1)
    for z in zs:
        for y in ys:
            for x in xs:
                if board[z][y][x] == 1:
                    if 2 <= adjacent_occupied(board, x, y, z) <= 3:
                        bp[z][y][x] = 1
                    else:
                        bp[z][y][x] = 0
                elif board[z][y][x] == 0:
                    if adjacent_occupied(board, x, y, z) == 3:
                        bp[z][y][x] = 1
                    else:
                        bp[z][y][x] = 0
    return bp

board = defaultdict(lambda: defaultdict(lambda: defaultdict(int)))

for x, l in enumerate(sys.stdin.readlines()):
    for y, c in enumerate(l.strip()):
        if c == '#':
            board[0][x][y] = 1
        else:
            board[0][x][y] = 0

#pprint(board)
#print(num_activated(evolve(evolve(evolve(evolve(evolve(evolve(board))))))))
for i in range(6):
    board = evolve(board)
    print(f"Done generation {i+1} ({num_activated(board)} activated)")
    #pprint(board)
#print(num_activated(board))
#pprint(evolve(evolve(board)))
#print(mmz(board), mmy(board), mmx(board))