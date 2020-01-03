import math

def loc(i, R, C):
    i = int(math.log2(i))
    return (i % C, i // C)

def pos(r, c, R, C):
    return 2**(C * c + r)


def valid_from(p, i, R, C):
    l = loc(i, R, C)
    for dx in [1, -1]:
        for dy in [1, -1]:
            #print("x range is %d, %d, %d" % (dx, -(l[0] + (1 - C) * (1 + dx)//2) + dx, dx))
            for r in range(dx, -(l[0] + (1 - C) * (1 + dx)//2) + dx, dx):
                #print("y range is %d, %d, %d" % (dy, -(l[1] + (1 - R) * (1 + dy)//2) + dy, dy))
                for c in range(dy, -(l[1] + (1 - R) * (1 + dy)//2) + dy, dy):
                    x, y = l[0] + r, l[1] + c
                    #print("considering (%d = %d + %d, %d = %d + %d)" % (x, l[0], r, y, l[1], c))
                    if abs(r) != abs(c) and (pos(x, y, R, C) & p) == 0:
                        yield (x,y)


def solve(R, C):
    q = set([(2**i, 2**i, (loc(2**i, R, C),)) for i in range(R * C)])

    while q:
        board, last_move, history = q.pop()
        for next_move in valid_from(board, last_move, R, C):
            new_board = board | pos(*next_move, R, C)
            new_history = history + (next_move,)
            #print(len(new_history))
            if len(new_history) == R * C:
                return "POSSIBLE", new_history
            q.add((new_board, pos(*next_move, R, C), new_history))
    
    return "IMPOSSIBLE", ()


if __name__ == "__main__":
    T = int(input())
    for i in range(1, T+1):
        R, C = map(int, input().split(" "))
        possible, history = solve(R, C)
        print("Case #%d: %s" % (i, possible))
        if possible:
            #print("history:", history)
            for x, y in history:
                print("%d %d" % (y + 1, x + 1))
