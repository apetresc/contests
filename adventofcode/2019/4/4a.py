def check(n):
    n = str(n)
    if sorted(n) != list(n):
        return False
    if not any([str(d) * 2 in n for d in range(10)]):
        return False
    return True


if __name__ == "__main__":
    a, b = map(int, input().split("-"))

    num_valid = 0
    for x in range(a, b+1):
        if check(x):
            num_valid += 1
            #print(x)
    print(num_valid)
