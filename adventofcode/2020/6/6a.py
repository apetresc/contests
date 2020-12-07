import sys

groups = [g.split() for g in sys.stdin.read().split('\n\n')]
total = 0
for group in groups:
    answers = set(''.join(group))
    total += len(answers)

print(total)