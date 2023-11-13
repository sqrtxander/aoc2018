from __future__ import annotations

import argparse
import os.path

import pytest

import support
from collections import defaultdict

INPUT_TXT = os.path.join(os.path.dirname(__file__), 'input.in')


class Worker:
    def __init__(self):
        self.time = 0
        self.task = None

    def assign(self, task: str, time: int) -> None:
        self.task = task
        self.time = time

    def tick(self) -> None:
        if self.time:
            self.time -= 1

    def __str__(self) -> str:
        return f'Worker(task={self.task}, time={self.time})'


def solve(s: str, worker_count: int, duration: int) -> int:
    lines = s.splitlines()

    steps = defaultdict(list)

    for line in lines:
        _, prereq, _, _, _, _, _, step, _, _ = line.split()
        steps[step].append(prereq)
        steps[prereq]

    workers = [Worker() for _ in range(worker_count)]
    time = 0
    reserved = set()
    while steps:
        for worker in workers:
            if not worker.time:
                if worker.task:
                    del steps[worker.task]
                    for prereqs in steps.values():
                        if worker.task in prereqs:
                            prereqs.remove(worker.task)
                    worker.task = None
                for step, prereqs in steps.items():
                    if not prereqs and step not in reserved:
                        reserved.add(step)
                        worker.assign(step, ord(step) -
                                      ord('A') + duration + 1)
                        break
            worker.tick()
        time += 1

    return time - 1


INPUT_S = '''\
Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
'''
EXPECTED = 15
WORKER_COUNT = 2
DURATION = 0


@pytest.mark.parametrize(
    ('input_s', 'worker_count', 'duration', 'expected'),
    (
        (INPUT_S, WORKER_COUNT, DURATION, EXPECTED),
    ),
)
def test(input_s: str, worker_count: int, duration: int, expected: int) -> None:
    assert solve(input_s, worker_count, duration) == expected


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument('data_file', nargs='?', default=INPUT_TXT)
    parser.add_argument('--worker-count', type=int, default=5)
    parser.add_argument('--duration', type=int, default=60)
    args = parser.parse_args()

    with open(args.data_file) as f, support.timing():
        print(solve(f.read(), args.worker_count, args.duration))

    return 0


if __name__ == '__main__':
    raise SystemExit(main())
