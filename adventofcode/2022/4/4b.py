import sys

print(len([1 for x in list(map(lambda x: list((map(lambda r: (list(map(lambda c: int(c.strip()), r.split('-')))), x.split(',')))), sys.stdin.readlines())) if max(x[0][0],x[1][0])<=min(x[0][1],x[1][1]) or 0]))
