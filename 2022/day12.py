with open("day12.txt", "r") as file:
    lines = file.readlines()

lines = [line[:-1] for line in lines]

pos = [0, 0]
target = [0, 0]

grid = []
for i, line in enumerate(lines):
    row = []
    
    for j, char in enumerate(line):
        if char == "E":
            target = [j, i]
        if char == "S":
            pos = [j, i]
            row.append(float('inf'))
            continue
            
        row.append(ord(char))
        
    grid.append(row)
    
''' Part 1
unvisited = [[i, j] for i in range(len(grid[0])) for j in range(len(grid))]
distances = [[float('inf') for i in range(len(grid[0]))] for j in range(len(grid))]
distances[pos[1]][pos[0]] = 0

while target in unvisited:
    neighbours = [[pos[0] - 1, pos[1]], [pos[0] + 1, pos[1]], [pos[0], pos[1] - 1], [pos[0], pos[1] + 1]]
    for neighbour in filter(lambda x: x in unvisited and grid[x[1]][x[0]] - grid[pos[1]][pos[0]] <= 1, neighbours):
        distances[neighbour[1]][neighbour[0]] = min(distances[neighbour[1]][neighbour[0]], distances[pos[1]][pos[0]] + 1)

    unvisited.remove([pos[0], pos[1]])
    next_pos = min(unvisited, key = lambda x: distances[x[1]][x[0]])
    pos = [next_pos[0], next_pos[1]]

print(distances[target[1]][target[0]])'''

trails = []
for y in range(len(grid)):
    for x in range(len(grid[0])):
        if grid[y][x] == ord('a'):
            pos = [x, y]
            unvisited = [[i, j] for i in range(len(grid[0])) for j in range(len(grid))]
            distances = [[float('inf') for i in range(len(grid[0]))] for j in range(len(grid))]
            distances[pos[1]][pos[0]] = 0

            while target in unvisited and not all(distances[u[1]][u[0]] == float('inf') for u in unvisited):
                neighbours = [[pos[0] - 1, pos[1]], [pos[0] + 1, pos[1]], [pos[0], pos[1] - 1], [pos[0], pos[1] + 1]]
                for neighbour in filter(lambda x: x in unvisited and grid[x[1]][x[0]] - grid[pos[1]][pos[0]] <= 1, neighbours):
                    distances[neighbour[1]][neighbour[0]] = min(distances[neighbour[1]][neighbour[0]], distances[pos[1]][pos[0]] + 1)

                unvisited.remove([pos[0], pos[1]])
                next_pos = min(unvisited, key = lambda x: distances[x[1]][x[0]])
                pos = [next_pos[0], next_pos[1]]

            trails.append(distances[target[1]][target[0]])

print(min(trails))
