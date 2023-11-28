def part1():
    with open("day22.txt", "r") as file:
        lines = file.readlines()
        grid, path = [list(line.strip("\n")) for line in lines[:-2]], list(lines[-1])

    direction = "R"
    position = [grid[0].index("."), 0]
    steps = 0
    while len(path) > 0 or steps > 0:
        if steps > 0:
            steps -= 1
            next_x, next_y = position[0], position[1]
            match direction:
                case "L":
                    grid[position[1]][position[0]] = "<"
                    next_x = position[0] - 1
                    if next_x < 0 or grid[position[1]][next_x] == " ":
                        next_x = max(i if grid[position[1]][i] != " " else -1 for i in range(len(grid[position[1]])))

                case "R":
                    grid[position[1]][position[0]] = ">"
                    next_x = position[0] + 1
                    if next_x >= len(grid[position[1]]) or grid[position[1]][next_x] == " ":
                        next_x = min(i if grid[position[1]][i] != " " else 9999999999 for i in range(len(grid[position[1]])))

                case "U":
                    grid[position[1]][position[0]] = "^"
                    col = [row[position[0]] if position[0] < len(row) else " " for row in grid]
                    next_y = position[1] - 1
                    if next_y < 0 or col[next_y] == " ":
                        next_y = max(i if col[i] != " " else -1 for i in range(len(col)))

                case "D":
                    grid[position[1]][position[0]] = "v"
                    col = [row[position[0]] if position[0] < len(row) else " " for row in grid]
                    next_y = position[1] + 1
                    if next_y >= len(col) or col[next_y] == " ":
                        next_y = min(i if col[i] != " " else 9999999999 for i in range(len(col)))

            if grid[next_y][next_x] != "#":
                position = [next_x, next_y]
            else:
                steps = 0

        else:
            char = path.pop(0)
            if char == "L":
                match direction:
                    case "L":
                        direction = "D"
                    case "R":
                        direction = "U"
                    case "U":
                        direction = "L"
                    case "D":
                        direction = "R"

            elif char == "R":
                match direction:
                    case "L":
                        direction = "U"
                    case "R":
                        direction = "D"
                    case "U":
                        direction = "R"
                    case "D":
                        direction = "L"
            
            else:
                steps = (steps * 10) + int(char)
                while len(path) > 0 and path[0] not in ("L", "R"):
                    char = path.pop(0)
                    steps = (steps * 10) + int(char)

    return (1000 * (position[1] + 1)) + (4 * (position[0] + 1)) + (("R", "D", "L", "U").index(direction))

def part2():
    with open("day22.txt", "r") as file:
        lines = file.readlines()
        grid, path = [list(line.strip("\n")) for line in lines[:-2]], list(lines[-1])

    direction = "R"
    position = [grid[0].index("."), 0]
    steps = 0
    while len(path) > 0 or steps > 0:
        if steps > 0:
            steps -= 1
            x, y = position[0], position[1]
            next_x, next_y = x, y
            next_direction = direction
            match direction:
                case "L":
                    grid[y][x] = "<"
                    next_x = x - 1
                    if next_x < 0 or grid[y][next_x] == " ":
                        if x >= 50 and x < 100 and y >= 0 and y < 50:
                            next_x = 0
                            next_y = 100 + (50 - y - 1)
                            next_direction = "R"
                        elif x >= 50 and x < 100 and y >= 50 and y < 100:
                            next_x = y - 50
                            next_y = 100
                            next_direction = "D"
                        elif x >= 0 and x < 50 and y >= 100 and y < 150:
                            next_x = 50
                            next_y = 50 - (y - 100) - 1
                            next_direction = "R"
                        elif x >= 0 and x < 50 and y >= 150 and y < 200:
                            next_x = 50 + (y - 150)
                            next_y = 0
                            next_direction = "D"

                case "R":
                    grid[y][x] = ">"
                    next_x = x + 1
                    if next_x >= len(grid[y]) or grid[y][next_x] == " ":
                        if x >= 100 and x < 150 and y >= 0 and y < 50:
                            next_x = 100 - 1
                            next_y = 100 + (50 - y - 1)
                            next_direction = "L"
                        elif x >= 50 and x < 100 and y >= 50 and y < 100:
                            next_x = 100 + (y - 50)
                            next_y = 50 - 1
                            next_direction = "U"
                        elif x >= 50 and x < 100 and y >= 100 and y < 150:
                            next_x = 150 - 1
                            next_y = 50 - (y - 100) - 1
                            next_direction = "L"
                        elif x >= 0 and x < 50 and y >= 150 and y < 200:
                            next_x = 50 + (y - 150)
                            next_y = 150 - 1
                            next_direction = "U"

                case "U":
                    grid[y][x] = "^"
                    col = [row[x] if x < len(row) else " " for row in grid]
                    next_y = y - 1
                    if next_y < 0 or col[next_y] == " ":
                        if x >= 50 and x < 100 and y >= 0 and y < 50:
                            next_x = 0
                            next_y = 150 + (x - 50)
                            next_direction = "R"
                        elif x >= 100 and x < 150 and y >= 0 and y < 50:
                            next_x = 50 - (x - 100) - 1
                            next_y = 200 - 1
                            next_direction = "U"
                        elif x >= 0 and x < 50 and y >= 100 and y < 150:
                            next_x = 50
                            next_y = 50 + (x - 50)
                            next_direction = "R"

                case "D":
                    grid[y][x] = "v"
                    col = [row[x] if x < len(row) else " " for row in grid]
                    next_y = y + 1
                    if next_y >= len(col) or col[next_y] == " ":
                        if x >= 100 and x < 150 and y >= 0 and y < 50:
                            next_x = 100 - 1
                            next_y = 50 + (x - 100)
                            next_direction = "L"
                        elif x >= 50 and x < 100 and y >= 100 and y < 150:
                            next_x = 50 - 1
                            next_y = 150 + (x - 50)
                            next_direction = "L"
                        elif x >= 0 and x < 50 and y >= 150 and y < 200:
                            next_x = 100 + x
                            next_y = 0
                            next_direction = "D"

            if grid[next_y][next_x] != "#":
                position = [next_x, next_y]
                direction = next_direction
            else:
                steps = 0

        else:
            char = path.pop(0)
            if char == "L":
                match direction:
                    case "L":
                        direction = "D"
                    case "R":
                        direction = "U"
                    case "U":
                        direction = "L"
                    case "D":
                        direction = "R"

            elif char == "R":
                match direction:
                    case "L":
                        direction = "U"
                    case "R":
                        direction = "D"
                    case "U":
                        direction = "R"
                    case "D":
                        direction = "L"
            
            else:
                steps = (steps * 10) + int(char)
                while len(path) > 0 and path[0] not in ("L", "R"):
                    char = path.pop(0)
                    steps = (steps * 10) + int(char)

    return (1000 * (position[1] + 1)) + (4 * (position[0] + 1)) + (("R", "D", "L", "U").index(direction))

print(f'Part 1: {part1()}')
print(f'Part 2: {part2()}')
