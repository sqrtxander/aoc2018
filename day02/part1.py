from __future__ import annotations

import argparse
import os.path

import pytest

import support
from collections import Counter

INPUT_TXT = os.path.join(os.path.dirname(__file__), 'input.in')


def solve(s: str) -> int:
    lines = s.splitlines()

    twos = 0
    threes = 0
    for line in lines:
        counts = Counter(line)
        twos += 2 in counts.values()
        threes += 3 in counts.values()
    return twos * threes


INPUT_S = '''\
abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab
'''
EXPECTED = 12


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
