def solve(N, path):
  return ''.join(['E' if c is 'S' else 'S' for c in path])


for i in range(1, int(input()) + 1):
  N = int(input())
  path = input()
  print("Case #%d: %s" % (i, solve(N, path)))
