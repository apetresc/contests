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
        return '(' + '|'.join(map(lambda c: ''.join(map(lambda x: convert_to_regex(RULES[x]), c)), clauses)) + ')'

matching = 0
for l in sys.stdin.readlines():
    if ':' in l:
        RULES[l.split(': ')[0]] = l.split(': ')[1].strip()
    elif l.strip() == "":
        # Compile pattern
        p = re.compile(convert_to_regex(RULES["0"]))
    else:
        # Validate message
        print(f"Comparing {l.strip()} against {p}: {p.fullmatch(l.strip())}")
        if p.fullmatch(l.strip()):
            matching += 1

print(convert_to_regex(RULES["0"]))
print(matching)
