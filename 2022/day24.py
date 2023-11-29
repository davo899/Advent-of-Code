from collections import deque
from math import lcm

start_blizzards = []

with open("day24.txt", "r") as file:
    lines = [line[:-1] for line in file.readlines()]
    for j, row in enumerate(lines):
        start_blizzards += [(i, j, char) for i, char in enumerate(row) if char in ("<", ">", "^", "v")]

walls = set([(1, -1), (len(lines[0]) - 2, len(lines))])
walls |= set((0, y) for y in range(len(lines)))
walls |= set((len(lines[0]) - 1, y) for y in range(len(lines)))
walls |= set((x, 0) for x in range(2, len(lines[0]) - 1))
walls |= set((x, len(lines) - 1) for x in range(1, len(lines[0]) - 2))

blizzards = {
    0: start_blizzards
}

cycle_time = lcm(len(lines[0]) - 2, len(lines) - 2)
result = -1
seen = set()
queue = deque([((1, 0), 1)])
first_goal = None
snacks = False
second_goal = None
while queue:
    state = queue.popleft()
    if state in seen:
        continue
    seen.add(state)

    position, time = state
    if not first_goal:
        if position == (len(lines[0]) - 2, len(lines) - 1):
            first_goal = time - 1
            seen = set()
            queue = deque([(position, time)])
            continue
    elif not snacks:
        if position == (1, 0):
            snacks = True
            seen = set()
            queue = deque([(position, time)])
            continue
    elif not second_goal:
        if position == (len(lines[0]) - 2, len(lines) - 1):
            second_goal = time - 1
            break

    mod_time = time % cycle_time
    if mod_time in blizzards:
        tblizzards = blizzards[mod_time]
    else:
        tblizzards = []
        for i, j, direction in blizzards[(mod_time - 1) % cycle_time]:
            match direction:
                case "<":
                    tblizzards.append((i - 1 if i > 1 else len(lines[0]) - 2, j, direction))
                case ">":
                    tblizzards.append((i + 1 if i < len(lines[0]) - 2 else 1, j, direction))
                case "^":
                    tblizzards.append((i, j - 1 if j > 1 else len(lines) - 2, direction))
                case "v":
                    tblizzards.append((i, j + 1 if j < len(lines) - 2 else 1, direction))
        blizzards[mod_time] = tblizzards

    x, y = position
    next_time = time + 1
    tblizzardsset = set((i, j) for i, j, _ in tblizzards)
    if not first_goal or snacks:
        for next_position in set([(x + 1, y), (x, y + 1)]) - tblizzardsset - walls:
            queue.appendleft((next_position, next_time))
        for next_position in set([(x, y), (x - 1, y), (x, y - 1)]) - tblizzardsset - walls:
            queue.append((next_position, next_time))
    else:
        for next_position in set([(x - 1, y), (x, y - 1)]) - tblizzardsset - walls:
            queue.appendleft((next_position, next_time))
        for next_position in set([(x, y), (x + 1, y), (x, y + 1)]) - tblizzardsset - walls:
            queue.append((next_position, next_time))

print(f"Part 1: {first_goal}")
print(f"Part 2: {second_goal}")
