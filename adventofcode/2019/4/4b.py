def check(n):
    n = str(n)
    if sorted(n) != list(n):
        return False
    for i in range(0, len(n) - 1):
        if n[i] == n[i+1] and (n+'x')[i-1] != n[i] and (n+'x')[i+2] != n[i]:
            return True
    return False


if __name__ == "__main__":
    a, b = map(int, input().split("-"))

    num_valid = 0
    for x in range(a, b+1):
        if check(x):
            num_valid += 1
            #print(x)
    print(num_valid)
