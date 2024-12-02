import sys


def check_report(deltas: list[int]) -> bool:
    return (
        (all([x > 0 for x in deltas]) or all([x < 0 for x in deltas]))
        and min(map(abs, deltas)) >= 1
        and max(map(abs, deltas)) <= 3
    )


def main():
    reports = []
    safe = 0
    for l in open(sys.argv[1], "r").readlines():
        reports.append(list(map(int, l.split())))

    for report in reports:
        for i in range(len(report)):
            dampened_report = report[0:i] + report[i + 1 :]
            deltas = list(
                map(lambda x: x[1] - x[0], zip(dampened_report, dampened_report[1:]))
            )
            if check_report(deltas):
                safe += 1
                break

    return safe


if __name__ == "__main__":
    print(main())
