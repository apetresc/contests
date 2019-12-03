m = 0

while True:
    try:
        l = input()
    except EOFError:
        break
    m += (int(l) // 3) - 2
print(m)
