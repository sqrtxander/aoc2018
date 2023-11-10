from __future__ import annotations

import argparse
import os.path

import pytest

import support
from datetime import datetime
from collections import defaultdict

INPUT_TXT = os.path.join(os.path.dirname(__file__), 'input.in')


def solve(s: str) -> int:

    lines = s.splitlines()
    lines.sort(key=lambda x: datetime.strptime(x[1:17], '%Y-%m-%d %H:%M'))

    guard = None
    sleeptime = defaultdict(int)
    for line in lines:
        time = datetime.strptime(line[1:17], '%Y-%m-%d %H:%M')
        if 'Guard' in line:
            guard = int(line.split()[3][1:])
        elif 'falls asleep' in line:
            asleep = time
        elif 'wakes up' in line:
            sleeptime[guard] += (time - asleep).total_seconds() // 60
        else:
            raise AssertionError(f'unrecognised line {line!r}')

    sleepy_guard = max(((g, t)
                       for g, t in sleeptime.items()), key=lambda x: x[1])[0]
    sleep_minutes = defaultdict(int)
    for line in lines:
        time = datetime.strptime(line[1:17], '%Y-%m-%d %H:%M')
        if 'Guard' in line:
            guard = int(line.split()[3][1:])
            continue

        if guard != sleepy_guard:
            continue

        if 'falls asleep' in line:
            asleep = time
        elif 'wakes up' in line:
            for m in range(asleep.minute, time.minute):
                sleep_minutes[m] += 1
        else:
            raise AssertionError(f'unrecognised line {line!r}')

    sleepy_minute = max(
        ((m, s) for m, s in sleep_minutes.items()), key=lambda x: x[1])[0]

    return sleepy_guard * sleepy_minute


INPUT_S = '''\
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up
'''
EXPECTED = 240


@pytest.mark.parametrize(
    ('input_s', 'expected'),
    (
        (INPUT_S, EXPECTED),
    ),
)
def test(input_s: str, expected: int) -> None:
    assert solve(input_s) == expected


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument('data_file', nargs='?', default=INPUT_TXT)
    args = parser.parse_args()

    with open(args.data_file) as f, support.timing():
        print(solve(f.read()))

    return 0


if __name__ == '__main__':
    raise SystemExit(main())
