# Author apetresc


def solve(a, b, n, S):
    print("YES" if S <= a * n + b and S % n <= b else "NO")


def main():
    N = int(input())
    for _ in range(N):
        # parse input
        a, b, n, S = map(int, input().split())
        solve(a, b, n, S)


if __name__ == '__main__':
    main()
