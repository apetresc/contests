import sys

def translate(s):
    return int(s[0:7 ].replace('F', '0').replace('B', '1'), 2), \
           int(s[7:11].replace('R', '1').replace('L', '0'), 2)


max_seat_id = 0
for l in sys.stdin:
    row, col = translate(l)
    seat_id = row * 8 + col
    max_seat_id = max(max_seat_id, seat_id)
print(max_seat_id)