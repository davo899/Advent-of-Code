def tower(n):
    with open("day17.txt", "r") as file:
        pushes = file.readline()

    rocks = [
        [
            (0, 0),
            (1, 0),
            (2, 0),
            (3, 0)
        ],
        [
            (1, 0),
            (0, 1),
            (1, 1),
            (2, 1),
            (1, 2)
        ],
        [
            (2, 0),
            (2, 1),
            (0, 2),
            (1, 2),
            (2, 2)
        ],
        [
            (0, 0),
            (0, 1),
            (0, 2),
            (0, 3)
        ],
        [
            (0, 0),
            (1, 0),
            (0, 1),
            (1, 1)
        ]
    ]

    top = -1
    grid = []
    push_index = -1
    rock_count = -1
    memory = {}
    added_height = 0
    while rock_count < n - 1:
        for _ in range(top + 10 - len(grid)):
            grid.append([False for _ in range(7)])

        rock_count += 1
        rock = rocks[rock_count % len(rocks)]

        root = [2, top + max(part[1] for part in rock) + 4]

        while True:
            push_index += 1
            push = pushes[push_index % len(pushes)]

            if push == "<" and all(root[0] + part[0] > 0 and not grid[root[1] - part[1]][root[0] + part[0] - 1] for part in rock):
                root[0] -= 1
            elif push == ">" and all(root[0] + part[0] < 6 and not grid[root[1] - part[1]][root[0] + part[0] + 1] for part in rock):
                root[0] += 1
            
            if any(root[1] - part[1] <= 0 or grid[root[1] - part[1] - 1][root[0] + part[0]] for part in rock):
                break

            root[1] -= 1

        for part in rock:
            y = root[1] - part[1]
            grid[y][root[0] + part[0]] = True
            if y > top:
                top = y

        if len(grid) > 30:
            current = (rock_count % len(rocks), push_index % len(pushes), tuple(tuple(row) for row in grid[top-10:top]))
            if current in memory:
                start_rock_count, start_top = memory[current]
                cycle_rock_count = rock_count - start_rock_count
                cycle_height = top - start_top
                cycles = (n - rock_count) // cycle_rock_count
                rock_count += cycles * cycle_rock_count
                added_height += cycles * cycle_height

            memory[current] = (rock_count, top)
        
        #for row in grid[::-1][:20]:
        #    for point in row:
        #        print("#" if point else ".", end="")
        #    print()
        #print()

    return top + added_height + 1

print(f"Part 1: {tower(2022)}")
print(f"Part 2: {tower(1_000_000_000_000)}")