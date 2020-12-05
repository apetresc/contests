import sys

def seat_id(s):
    return 8 * int(s[0:7 ].replace('F', '0').replace('B', '1'), 2) + \
           int(s[7:11].replace('R', '1').replace('L', '0'), 2)


seat_ids = list(map(seat_id, sys.stdin))
print(set(range(min(seat_ids), max(seat_ids))).difference(set(seat_ids)))