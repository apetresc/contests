n = int(input())

for _ in range(n):
    x = int(input())
    print("%d is " % x + ("even" if x % 2 == 0 else "odd"))
