from collections import defaultdict
import sys

def parse_rule_forward(l):
    s, ps = l[:-2].split(' contain ')
    ds = []
    for p in ps.split(', '):
        if p != 'no other bags':
            ds.append((p.split()[0], ' '.join(p.split()[1:3])))
    return ' '.join(s.split()[0:2]), ds

def parse_rule_backward(l):
    s, ps = l[:-2].split(' contain ')
    for p in ps.split(', '):
        if p != 'no other bags':
            #ds.append((p.split()[0], ' '.join(p.split()[1:3])))
            yield ' '.join(p.split()[1:3]), ' '.join(s.split()[0:2])


forward_rules = {}
backward_rules = defaultdict(list)
for l in sys.stdin.readlines():
    f, t = parse_rule_forward(l)
    forward_rules[f] = t
    for f, t in parse_rule_backward(l):
        backward_rules[f].append(t)
print(forward_rules)
print(backward_rules)

seen = set()
q = ['shiny gold']
while q:
    c = q.pop()
    for x in backward_rules[c]:
        seen.add(x)
        q.append(x)
print(seen)
print(len(seen))