pieces = input().split()
print(' '.join([str([1, 1, 2, 2, 2, 8][x] - int(pieces[x])) for x in range(len(pieces))]))
