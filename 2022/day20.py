with open("day20.txt", "r") as file:
    ns = [int(line) for line in file.readlines()]

indices = [i for i in range(len(ns))]

for i in range(len(ns)):
    j = indices.index(i)
    index = j + ns[i]
    indices.remove(i)
    indices.insert(index % len(indices), i)

zero_index = indices.index(ns.index(0))

print(f"Part 1: {sum(ns[indices[(zero_index + jump) % len(ns)]] for jump in (1000, 2000, 3000))}")

ns = [811589153 * n for n in ns]
indices = [i for i in range(len(ns))]

for _ in range(10):
    for i in range(len(ns)):
        j = indices.index(i)
        index = j + ns[i]
        indices.remove(i)
        indices.insert(index % len(indices), i)

zero_index = indices.index(ns.index(0))

print(f"Part 2: {sum(ns[indices[(zero_index + jump) % len(ns)]] for jump in (1000, 2000, 3000))}")
