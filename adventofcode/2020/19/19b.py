import re
import sys

RULES = {}

def convert_to_regex(r):
    if r == "\"a\"":
        return 'a'
    elif r == "\"b\"":
        return 'b'
    else:
        clauses = map(lambda x: x.split(), r.split("|"))
        return '(?:' + '|'.join(map(lambda c: ''.join(map(lambda x: convert_to_regex(RULES[x]), c)), clauses)) + ')'

matching = 0
for l in sys.stdin.readlines():
    if ':' in l:
        RULES[l.split(': ')[0]] = l.split(': ')[1].strip()
    elif l.strip() == "":
        # Compile pattern
        r31 = convert_to_regex(RULES["31"])
        r42 = convert_to_regex(RULES["42"])
        r0s = [re.compile(r42 + "+" + (r42 * x) + (r31 * x)) for x in range(1, 100)]
    else:
        # Validate message
        for p in r0s:
            if p.fullmatch(l.strip()):
                matching += 1
                break

print(matching)
