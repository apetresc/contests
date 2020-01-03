import math

def gcd(a, b):
  if b == 0:
    return a
  else:
    return gcd(b, a % b)

def solve(C):
  Ps = set()
  letters = []
  k, p = 0, 1
  while p == 1:
    # We have an ambiguous case
    p = C[k] // gcd(C[k], C[k+1])
    k += 1
  if p == 1:
    p = int(math.sqrt(C[0]))
  Ps.add(p)
  letters.append(p)
  for c in C:
    #print("Letters so far: %s" % letters)
    pp = c // p
    if pp == 1:
      pp = int(math.sqrt(c))
    p = pp
    Ps.add(p)
    letters.append(p)
  Ps = sorted(list(Ps))
  cipher = {Ps[i]: chr(ord('A') + i) for i in range(len(Ps))} 
  return ''.join([cipher[c] for c in letters])


for i in range(1, int(input()) + 1):
  N, L = input().split(" ")
  C = list(map(int, input().split(" ")))
  print("Case #%d: %s" % ((i, solve(C))))
