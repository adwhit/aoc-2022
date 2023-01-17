from __future__ import annotations
from typing import List, Callable
from collections import deque


class Monkey:
    def __init__(
        self,
        items: List[int],
        operation: Callable[[int], int],
        test_divisor: int,
        if_true: int,
        if_false: int,
    ):
        self.items = deque(items)
        self.operation = operation
        self.test_divisor = test_divisor
        self.if_true = if_true
        self.if_false = if_false
        self.inspect_count = 0
        self.big_divisor = None

    def handle_bag(self, monkeys: List[Monkey], moderate: bool):
        worry = self.operation(self.items.popleft())
        if moderate:
            worry //= 3
        if self.big_divisor is not None:
            worry %= self.big_divisor
        if worry % self.test_divisor == 0:
            next_monkey_ix = self.if_true
        else:
            next_monkey_ix = self.if_false
        next_monkey = monkeys[next_monkey_ix]
        next_monkey.items.append(worry)
        self.inspect_count += 1


def round(monkeys: List[Monkey], moderate: bool):
    for m in monkeys:
        while len(m.items) > 0:
            m.handle_bag(monkeys, moderate)

def twenty_rounds(monkeys: List[Monkey]):
    for _ in range(20):
        round(monkeys, True)
        # for m in monkeys:
        #     print(m.items)
        # print()

def ten_thousand_rounds(monkeys: List[Monkey]):
    for _ in range(10000):
        round(monkeys, False)

def data() -> List[Monkey]:
    return  [
        Monkey([54, 61, 97, 63, 74],             lambda x: x * 7,  17, 5, 3),
        Monkey([61, 70, 97, 64, 99, 83, 52, 87], lambda x: x + 8,  2,  7, 6),
        Monkey([60, 67, 80, 65],                 lambda x: x * 13, 5,  1, 6),
        Monkey([61, 70, 76, 69, 82, 56],         lambda x: x + 7,  3,  5, 2),
        Monkey([79, 98],                         lambda x: x + 2,  7,  0, 3),
        Monkey([72, 79, 55],                     lambda x: x + 1,  13, 2, 1),
        Monkey([63],                             lambda x: x + 4,  19, 7, 4),
        Monkey([72, 51, 93, 63, 80, 86, 81],     lambda x: x * x,  11, 0, 4),
    ]

    # return [
    #     Monkey([79, 98],             lambda x: x * 19, 23, 2, 3),
    #     Monkey([54, 65, 75, 74],     lambda x: x + 6, 19,  2, 0),
    #     Monkey([79, 60, 97],         lambda x: x * x, 13, 1, 3),
    #     Monkey([74],                 lambda x: x + 3, 17, 0, 1),
    # ]

def best(monkeys: List[Monkey]):
    [f, s] = sorted((m.inspect_count for m in monkeys), reverse=True)[:2]
    print(f * s)

p1 = data()
twenty_rounds(p1)
best(p1)

p2 = data()
bigd = 1
for m in p2:
    bigd *= m.test_divisor
for m in p2:
    m.big_divisor = bigd
ten_thousand_rounds(p2)
best(p2)
