def solve(N):
  a = int(str(N).replace('4', '3'))
  return a, N - a

for i in range(1, input() + 1):
  print("Case #%d: %d %d" % ((i,) + solve(input())))
