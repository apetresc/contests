import re
import sys

def apply_decoder(mask, l):
    lm = ''.join(['{0:036b}'.format(l)[i] if mask[i] == '0' else mask[i] for i in range(36)])
    locs = [35 - i for i, c in enumerate(lm) if c == 'X'][::-1]
    for i in range(2**len(locs)):
        m, j = 0, 0
        while i:
            if i % 2 == 1:
                m |= 2**locs[j]
            j += 1
            i //= 2
        yield m | int(lm.replace('X', '0'), 2)

mem = {}
for l in sys.stdin:
    if m := re.match(r'mask = (.*)', l):
        mask = m.groups()[0]
    elif m:= re.match(r'mem\[(\d+)\] = (\d+)', l):
        l, v = map(int, m.groups()[0:2])
        for ll in apply_decoder(mask, l):
            mem[ll] = v

print(sum(mem.values()))