#!/usr/bin/env python3

with open("../data/day3.data") as f:
    rows = list(l.strip() for l in f)

def priority(val):
    if val.upper() == val:
        return ord(val) - ord("A") + 27
    else:
        return ord(val) - ord("a") + 1

total = 0
for line in rows:
    left = set(line[:len(line)//2])
    right = set(line[len(line)//2:])
    common = left & right
    assert len(common) == 1
    val = priority(common.pop())
    total += val
print(total)

ix = 0
total2 = 0
while ix < len(rows):
    common = set(rows[ix]) & set(rows[ix + 1]) &  set(rows[ix + 2])
    ix += 3
    assert len(common) == 1
    total2 += priority(common.pop())
print(total2)
