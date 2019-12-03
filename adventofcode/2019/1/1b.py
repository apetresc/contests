m = 0

while True:
    try:
        l = input()
    except EOFError:
        break
    d = (int(l) // 3) - 2
    while d > 0:
        m += d
        d = (d // 3) - 2
print(m)
