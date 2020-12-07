from collections import defaultdict
import sys

def parse_rule_forward(l):
    s, ps = l[:-2].split(' contain ')
    ds = []
    for p in ps.split(', '):
        if p != 'no other bags':
            ds.append((int(p.split()[0]), ' '.join(p.split()[1:3])))
    return ' '.join(s.split()[0:2]), ds

forward_rules = {}
for l in sys.stdin.readlines():
    f, t = parse_rule_forward(l)
    forward_rules[f] = t
print(forward_rules)


def contains(quantity, color):
    s = 1
    for rule in forward_rules[color]:
        s += contains(*rule)
    return quantity * s

print(contains(1, 'shiny gold') - 1)