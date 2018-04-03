N = int(input())
s = 0

for i in range(N):
    Pi = input()
    s += int(Pi[0:-1]) ** int(Pi[-1:])

print(s)
