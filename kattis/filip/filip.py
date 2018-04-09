A, B = map(int, input().split())

def flip(s):
    return ''.join([c for c in str(s)][::-1])

print(max(flip(A), flip(B)))

