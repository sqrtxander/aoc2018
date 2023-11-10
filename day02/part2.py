from __future__ import annotations

import argparse
import os.path

import pytest

import support
from collections import Counter, defaultdict

INPUT_TXT = os.path.join(os.path.dirname(__file__), 'input.in')


def solve(s: str) -> int:
    lines = s.splitlines()

    for i, line_1 in enumerate(lines):
        for line_2 in lines[i + 1:]:
            if sum(1 for a, b in zip(line_1, line_2) if a != b) == 1:
                return ''.join(a for a, b in zip(line_1, line_2) if a == b)
    raise AssertionError('No solution found')
        

INPUT_S = '''\
abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz
'''
EXPECTED = 'fgij'


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

