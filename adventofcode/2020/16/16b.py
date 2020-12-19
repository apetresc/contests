import math
import sys

def parse_rule(l : str):
    field, intervals = l.split(': ')
    return field, [(lambda x: (int(x[0]), int(x[1])))(interval.split('-')) for interval in intervals.split(' or ')]


rules = {}
while line := sys.stdin.readline().strip():
    field, intervals = parse_rule(line.strip())
    rules[field] = intervals

sys.stdin.readline()
your_ticket = list(map(int, sys.stdin.readline().split(',')))

sys.stdin.readline() ; sys.stdin.readline()
nearby_tickets = []
for l in sys.stdin.readlines():
    nearby_tickets.append(list(map(int, l.split(','))))

potential = [set(rules.keys())] * len(rules)
for nearby_ticket in nearby_tickets:
    if all([any([i[0] <= f <= i[1] for i in sum(rules.values(), [])]) for f in nearby_ticket]):
        for i, f in enumerate(nearby_ticket):
            potential[i] = potential[i].intersection(set(k for k in rules.keys() if any([r[0] <= f <= r[1] for r in rules[k]])))

while True:
    done = True
    for match in [p for p in potential if len(p) == 1]:
        for p in potential:
            if len(p) > 1 and match.issubset(p):
                p.difference_update(match)
                done = False
    if done:
        break
print([(potential[i], your_ticket[i]) for i in range(len(potential))])
print(math.prod([your_ticket[i] for i in range(len(potential)) if 'departure' in list(potential[i])[0]]))