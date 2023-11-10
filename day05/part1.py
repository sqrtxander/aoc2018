from __future__ import annotations

import argparse
import os.path

import pytest

import support
import re

INPUT_TXT = os.path.join(os.path.dirname(__file__), 'input.in')


def solve(s: str) -> int:

    lst = list(s.strip())
    prev_len = 0
    while len(lst) != prev_len:
        prev_len = len(lst)
        lst = react(lst)
    return len(lst)


def react(lst: list[int]) -> list[int]:
    i = 0
    while i < len(lst) - 1:
        if lst[i] != lst[i + 1] and lst[i].lower() == lst[i + 1].lower():
            del lst[i:i + 2]
            i = max(0, i - 1)
        else:
            i += 1
    return lst


INPUT_S = '''\
dabAcCaCBAcCcaDA
'''
EXPECTED = 10


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
