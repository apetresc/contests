ps = range(1, 11)
while True:
    guess = int(input())
    if guess == 0:
        break

    response = input()

    if response == 'right on':
        print("Stan may be honest" if guess in ps else "Stan is dishonest")
        ps = range(1, 11)
    elif response == 'too high':
        ps = [p for p in ps if p < int(guess)]
    elif response == 'too low':
        ps = [p for p in ps if p > int(guess)]

