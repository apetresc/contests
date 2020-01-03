from random import randint
import multiprocessing


def rand_digit_not_4(max=None):
  n = randint(0, max if max else 9)
  while n == 4:
    n = randint(0, max if max else 9)
  return str(n)

def solve(N):
  Ns = str(N)
  b = 4
  attempts = 0

  while True:
    a = int(''.join([rand_digit_not_4(max=int(Ns[0]) - 1)] + [rand_digit_not_4() for _ in range(len(Ns) - 1)]))
    b = N - a
    attempts += 1
    if '4' not in str(b):
      break

  print("There were %d attempts" % attempts)
  return (a, b)


T = input()
Nl = [input() for i in range(1, T+1)]
pool = multiprocessing.Pool()

solutions = pool.map(solve, Nl)

for i in range(1, T + 1):
  print("Case #%d: %d %d" % ((i,) + solutions[i - 1]))
