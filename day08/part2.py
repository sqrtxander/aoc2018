from __future__ import annotations

import argparse
import os.path

import pytest

import support

INPUT_TXT = os.path.join(os.path.dirname(__file__), 'input.in')


class Node:
    def __init__(self):
        self.children = []
        self.metadata = []


class Tree:
    def __init__(self, root: Node) -> None:
        self.root = root

    def sum_values(self) -> int:
        def sum_values_node(node: Node) -> int:
            if not node.children:  # no children
                return sum(node.metadata)

            return sum(sum_values_node(node.children[i-1]) 
                       for i in node.metadata if 1 <= i <= len(node.children))

        return sum_values_node(self.root)

    def __str__(self):
        return str(self.root)

def solve(s: str) -> int:
    def parse_nodes(l: list[int], idx: int) -> tuple[Node, int]:
        child_count = l[idx]
        metadata_count = l[idx + 1]
        node = Node()
        for _ in range(child_count):
            idx += 2
            child, idx = parse_nodes(l, idx)
            node.children.append(child)
        node.metadata = l[idx + 2: idx + 2 + metadata_count]

        return node, idx + metadata_count

    nums = support.parse_numbers_split(s)
    tree = Tree(parse_nodes(nums, 0)[0])
    return tree.sum_values()


INPUT_S = '''\
2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
'''
EXPECTED = 66


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
