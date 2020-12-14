import math
import re
import sys

def apply_mask(mask, v):
    for i in range(36)[::-1]:
        if mask[-i-1] == '1':
            v = v | (2**i)
        elif mask[-i-1] == '0':
            v = v & ((2**36 - 1) - 2**i)
    return v

mem = {}

for l in sys.stdin:
    if m := re.match(r'mask = (.*)', l):
        mask = m.groups()[0]
    elif m:= re.match(r'mem\[(\d+)\] = (\d+)', l):
        l, v = map(int, m.groups()[0:2])
        mem[l] = apply_mask(mask, v)

print(mem)
print(sum(mem.values()))