def parse_opcode(l, p):
    if l[p] == 1:
        l[l[p + 3]] = l[l[p + 1]] + l[l[p + 2]]
    elif l[p] == 2:
        l[l[p + 3]] = l[l[p + 1]] * l[l[p + 2]]
    elif l[p] == 99:
        return False
    else:
        raise ValueError("Invalid opcode %d at position %d" % (l[p], p))
    #print(l)
    return True

l = list(map(int, input().split(",")))
p = 0
while parse_opcode(l, p):
    p += 4
print("Done:", l)
