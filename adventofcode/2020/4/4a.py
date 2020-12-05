import sys


REQUIRED_FIELDS = set([
    'byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'
])
OPTIONAL_FIELDS = set(['cid'])

lines = sys.stdin.read()
valid = 0
for passport in lines.split('\n\n'):
    fields = dict(line.split(':') for line in passport.split())
    if REQUIRED_FIELDS.intersection(set(fields.keys())) == REQUIRED_FIELDS:
        valid += 1
    print(fields)
print(valid)
