from functools import reduce
import sys

groups = [g.split() for g in sys.stdin.read().split('\n\n')]
total = 0
for group in groups:
    answers = set(group[0])
    for p in group[1:]:
        answers = answers.intersection(set(p))
    total += len(answers)

print(total)