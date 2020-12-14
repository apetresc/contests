import math
import re
import sys

def apply_decoder(mask, l):
    lm = '{0:036b}'.format(l)
    lm = ''.join([lm[i] if mask[i] == '0' else mask[i] for i in range(36)])
    #print(f"l={lm}")
    locs = [35 - i for i, c in enumerate(lm) if c == 'X'][::-1]
    for i in range(2**len(locs)):
        m = 0
        j = 0
        while i:
            if i % 2 == 1:
                m = m | 2**locs[j]
            j += 1
            i //= 2
        #print("??{0:036b}".format(m))
        #print("=={0:036b}={0}".format(m | int(lm.replace('X', '0'), 2)))
        yield m | int(lm.replace('X', '0'), 2)

mem = {}

for l in sys.stdin:
    if m := re.match(r'mask = (.*)', l):
        mask = m.groups()[0]
    elif m:= re.match(r'mem\[(\d+)\] = (\d+)', l):
        l, v = map(int, m.groups()[0:2])
        for ll in apply_decoder(mask, l):
            mem[ll] = v

#print(mem)
print(sum(mem.values()))