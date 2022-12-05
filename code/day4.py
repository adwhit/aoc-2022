total = 0
total2 = 0
with open("../data/day4.data") as f:
    for row in f:
        row = row.strip()
        [l, r] = row.split(',')
        [ll, lr] = l.split('-')
        [rl, rr] = r.split('-')
        lset = set(range(int(ll), int(lr) + 1))
        rset = set(range(int(rl), int(rr) + 1))
        if len(lset.difference(rset)) == 0 or len(rset.difference(lset)) == 0:
            total += 1
        if len(lset | rset) < len(lset) + len(rset):
            total2 += 1
print(total)
print(total2)
