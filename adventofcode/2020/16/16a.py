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

#print(rules, your_ticket, nearby_tickets)

invalid_values = []
for nearby_ticket in nearby_tickets:
    for f in nearby_ticket:
        if not any([i[0] <= f <= i[1] for i in sum(rules.values(), [])]):
            invalid_values.append(f)
print(sum(invalid_values))