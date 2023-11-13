from __future__ import annotations

import argparse
import os.path

import pytest

import support
from collections import defaultdict

INPUT_TXT = os.path.join(os.path.dirname(__file__), 'input.in')


def solve(s: str) -> int:
    lines = s.splitlines()

    steps = defaultdict(list)

    for line in lines:
        _, prereq, _, _, _, _, _, step, _, _ = line.split()
        steps[step].append(prereq)
        steps[prereq]

    order = ''
    while steps:
        available = [step for step in steps if not steps[step]]
        chosen = min(available)
        order += chosen
        del steps[chosen]
        for step in steps:
            if chosen in steps[step]:
                steps[step].remove(chosen)
    return order


INPUT_S = '''\
Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
'''
EXPECTED = 'CABDFE'


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
