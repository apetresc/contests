import sys

def at(board, x, y):
    if x < 0 or x >= len(board) or y < 0 or y >= len(board[x]):
        return '.'
    else:
        return board[x][y]

def adjacent_occupied(board, x, y):
    adj = 0
    for dx in range(-1, 2):
        for dy in range(-1, 2):
            if dx == dy == 0:
                continue
            if at(board, x + dx, y + dy) == '#':
                adj += 1
    return adj


def evolve(board):
    bp = []
    for y in range(len(board)):
        bp.append([])
        for x in range(len(board[y])):
            if board[y][x] == 'L' and adjacent_occupied(board, y, x) == 0:
                bp[y].append('#')
            elif board[y][x] == '#' and adjacent_occupied(board, y, x) >= 4:
                bp[y].append('L')
            else:
                bp[y].append(board[y][x])
    return bp

def pprint(board):
    for l in board:
        print(''.join(l))

def pprint_adj(board):
    for y in range(len(board)):
        for x in range(len(board[y])):
            print(adjacent_occupied(board, y, x), end='')
        print()

board = [l.strip() for l in sys.stdin.readlines()]

while True:
    bp = evolve(board)
    pprint(bp)
    print()
    if bp == board:
        print(sum([row.count('#') for row in bp]))
        sys.exit()
    else:
        board = bp