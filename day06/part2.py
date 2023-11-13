from __future__ import annotations

import argparse
import os.path

import pytest

import support

INPUT_TXT = os.path.join(os.path.dirname(__file__), 'input.in')


def solve(s: str, req_distance: int) -> int:
    points = {support.parse_point_comma(line)
              for line in s.strip().splitlines()}
    min_x = min(x for x, _ in points)
    max_x = max(x for x, _ in points)
    min_y = min(y for _, y in points)
    max_y = max(y for _, y in points)
    area_size = 0
    for x in range(min_x, max_x + 1):
        for y in range(min_y, max_y + 1):
            total = sum(abs(x - i) + abs(y - j) for i, j in points)
            if total < req_distance:
                area_size += 1
    return area_size


INPUT_S = '''\
1, 1
1, 6
8, 3
3, 4
5, 5
8, 9
'''
REQ_DISTANCE = 32
EXPECTED = 16


@pytest.mark.parametrize(
    ('input_s', 'req_distance', 'expected'),
    (
        (INPUT_S, REQ_DISTANCE, EXPECTED),
    ),
)
def test(input_s: str, req_distance: int, expected: int) -> None:
    assert solve(input_s, req_distance) == expected


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument('data_file', nargs='?', default=INPUT_TXT)
    parser.add_argument('--req-distance', type=int, default=10000)
    args = parser.parse_args()

    with open(args.data_file) as f, support.timing():
        print(solve(f.read(), args.req_distance))

    return 0


if __name__ == '__main__':
    raise SystemExit(main())
