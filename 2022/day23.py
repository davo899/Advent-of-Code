def part1():
    elves = set()

    with open("day23.txt", "r") as file:
        lines = file.readlines()
        for j, line in enumerate(lines):
            elves |= set((i, j) for i in range(len(line)) if line[i] == "#")

    for round_ in range(10):
        proposed = []
        for x, y in elves:
            if any((x + i, y + j) in elves for i, j in ((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))):
                found_valid = False
                for i in range(4):
                    match (i + round_) % 4:
                        case 0:
                            checks = ((-1, -1), (0, -1), (1, -1))
                            nx, ny = (0, -1)
                        case 1:
                            checks = ((-1, 1), (0, 1), (1, 1))
                            nx, ny = (0, 1)
                        case 2:
                            checks = ((-1, -1), (-1, 0), (-1, 1))
                            nx, ny = (-1, 0)
                        case 3:
                            checks = ((1, -1), (1, 0), (1, 1))
                            nx, ny = (1, 0)

                    if all((x + cx, y + cy) not in elves for cx, cy in checks):
                        found_valid = True
                        break

                if found_valid:
                    proposed.append(((x, y), (x + nx, y + ny)))

        for elf, next_ in proposed:
            if sum(1 if cnext == next_ else 0 for _, cnext in proposed) == 1:
                elves.remove(elf)
                elves.add(next_)

    x1 = min(x for x, _ in elves)
    y1 = min(y for _, y in elves)
    x2 = max(x + 1 for x, _ in elves)
    y2 = max(y + 1 for _, y in elves)
    return len(set((x1 + x, y1 + y) for x in range(x2 - x1) for y in range(y2 - y1)) - elves)

def part2():
    elves = set()

    with open("day23.txt", "r") as file:
        lines = file.readlines()
        for j, line in enumerate(lines):
            elves |= set((i, j) for i in range(len(line)) if line[i] == "#")

    round_ = 0
    while True:
        proposed = {}
        for x, y in elves:
            if elves & set([(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1, y+1)]):
                found_valid = False
                for i in range(4):
                    match (i + round_) % 4:
                        case 0:
                            checks = set([(x-1, y-1), (x, y-1), (x+1, y-1)])
                            next_ = (x, y-1)
                        case 1:
                            checks = set([(x-1, y+1), (x, y+1), (x+1, y+1)])
                            next_ = (x, y+1)
                        case 2:
                            checks = set([(x-1, y-1), (x-1, y), (x-1, y+1)])
                            next_ = (x-1, y)
                        case 3:
                            checks = set([(x+1, y-1), (x+1, y), (x+1, y+1)])
                            next_ = (x+1, y)

                    if not elves & checks:
                        found_valid = True
                        break

                if found_valid:
                    proposed[next_] = (True, (x, y)) if next_ not in proposed else (False, None)

        moved = False
        for next_, moving in proposed.items():
            if moving[0]:
                elves.remove(moving[1])
                elves.add(next_)
                moved = True

        if not moved:
            return round_ + 1
        
        round_ += 1

print(f"Part 1: {part1()}")
print(f"Part 2: {part2()}")
