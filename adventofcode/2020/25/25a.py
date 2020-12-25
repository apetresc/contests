#DOOR_KEY = 17807724
#CARD_KEY = 5764801
CARD_KEY = 8335663
DOOR_KEY = 8614349


def transform(s, loop_size):
    return pow(s, loop_size, 20201227)

l = 1
while True:
    if (k := transform(7, l)) in [CARD_KEY, DOOR_KEY]:
        print(transform(DOOR_KEY if k == CARD_KEY else CARD_KEY, l))
        break
    l += 1