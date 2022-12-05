import sys

print(len(list( [(x[0], x[1]) for x in list(map(lambda x: (list(map(lambda r: (list(map(lambda c: int(c.strip()), r.split('-')))), x.split(',')))), sys.stdin.readlines())) if [min(x[0][0], x[1][0]), max(x[0][1], x[1][1])] in x])))
