from __future__ import annotations

import argparse
import os.path

import pytest

import support
import re
from collections import defaultdict

INPUT_TXT = os.path.join(os.path.dirname(__file__), 'input.in')


def solve(s: str) -> int:
    lines = s.splitlines()
    seen = defaultdict(int)
    for line in lines:
        x, y, w, h = map(int, re.fullmatch(
            r'#\d+ @ (\d+),(\d+): (\d+)x(\d+)', line).groups())
        for i in range(x, x + w):
            for j in range(y, y + h):
                seen[(i, j)] += 1

    return sum(v > 1 for v in seen.values())


INPUT_S = '''\
#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2
'''
EXPECTED = 4


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
