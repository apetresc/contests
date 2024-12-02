import sys


def main():
    reports = []
    safe = 0
    for l in open(sys.argv[1], "r").readlines():
        reports.append(list(map(int, l.split())))

    for report in reports:
        deltas = list(map(lambda x: x[1] - x[0], zip(report, report[1:])))
        if (
            (all([x > 0 for x in deltas]) or all([x < 0 for x in deltas]))
            and min(map(abs, deltas)) >= 1
            and max(map(abs, deltas)) <= 3
        ):
            safe += 1

    return safe


if __name__ == "__main__":
    print(main())
