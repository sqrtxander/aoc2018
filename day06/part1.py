from __future__ import annotations

import argparse
import os.path

import pytest

import support

INPUT_TXT = os.path.join(os.path.dirname(__file__), 'input.in')


def solve(s: str) -> int:
    def is_finite(point):
        for x, y in closest_points:
            if x not in (min_x, max_x) or y not in (min_y, max_y):
                continue
            if closest_points[(x, y)] == point:
                return False
        return True

    points = {support.parse_point_comma(line)
              for line in s.strip().splitlines()}
    min_x = min(x for x, _ in points)
    max_x = max(x for x, _ in points)
    min_y = min(y for _, y in points)
    max_y = max(y for _, y in points)
    closest_points = {}
    for x in range(min_x, max_x + 1):
        for y in range(min_y, max_y + 1):
            closest = None
            closest_distance = None
            for p_x, p_y in points:
                distance = abs(x - p_x) + abs(y - p_y)
                if closest_distance is None or distance < closest_distance:
                    closest = (p_x, p_y)
                    closest_distance = distance
                elif distance == closest_distance:
                    closest = None
            closest_points[(x, y)] = closest

    finite_areas = {point: list(closest_points.values()).count(
        point) for point in points if is_finite(point)}
    return max(finite_areas.values())


INPUT_S = '''\
1, 1
1, 6
8, 3
3, 4
5, 5
8, 9
'''
EXPECTED = 17


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
