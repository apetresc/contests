import re
import sys

def validate_height(s):
    n, unit = re.fullmatch(r'(\d+)([a-z]+)', s).groups()
    return (unit == 'cm' and (150 <= int(n) <= 193)) or (unit == 'in' and (59 <= int(n) <= 76))

def validate_fields(passport):
    if not set(REQUIRED_FIELDS.keys()).issubset(set(passport.keys())):
        # We have some missing required fields
        return False
    if not set(passport.keys()).issubset(set(REQUIRED_FIELDS.keys()).union(set(OPTIONAL_FIELDS.keys()))):
        # We have some extraneous fields. Not sure if this is invalid or not.
        return False
    for field in REQUIRED_FIELDS.keys():
        try:
            if REQUIRED_FIELDS[field](passport[field]):
                print("valid:", field, passport[field])
            else:
                print("invalid:", field, passport[field])
                return False
        except:
            print("invalid:", field, passport[field])
            return False
    return True

REQUIRED_FIELDS = {
    'byr': lambda x: (len(x) == 4) and 1920 <= int(x) <= 2002,
    'iyr': lambda x: (len(x) == 4) and 2010 <= int(x) <= 2020,
    'eyr': lambda x: (len(x) == 4) and 2020 <= int(x) <= 2030,
    'hgt': validate_height,
    'hcl': lambda x: re.fullmatch(r'#[0-9a-f]{6}', x) != None,
    'ecl': lambda x: x in ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'],
    'pid': lambda x: re.fullmatch(r'\d{9}', x) != None,
}
OPTIONAL_FIELDS = {
    'cid': lambda x: True
}

lines = sys.stdin.read().strip()
valid = 0
for passport in lines.split('\n\n'):
    fields = dict(line.split(':') for line in passport.split())
    print(fields)
    if validate_fields(fields):
        print("✅")
        valid += 1
    else:
        print("❌")
print(valid)
