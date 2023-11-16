from __future__ import annotations

import argparse
import os.path

import pytest

import support
from collections import deque

INPUT_TXT = os.path.join(os.path.dirname(__file__), 'input.in')


def solve(s: str) -> int:
    words = s.split()
    player_count = int(words[0])
    last_marble = int(words[6])

    scores = [0] * player_count
    circle = deque([0])
    marble = 1
    player = 0
    curr_idx = 0
    for marble in range(1, last_marble + 1):
        if marble % 23 != 0:
            circle.rotate(-1)
            circle.append(marble)
        else:
            scores[player] += marble
            circle.rotate(7)
            scores[player] += circle.pop()
            circle.rotate(-1)
        player = (player + 1) % player_count

    return max(scores)


INPUT_S = '''\
30 players; last marble is worth 5807 points
'''
EXPECTED = 37305


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
