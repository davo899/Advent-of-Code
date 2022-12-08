with open("day8.txt", "r") as file:
    lines = file.readlines()

lines = [line[:-1] for line in lines]

grid = []
for line in lines:
    row = []
    for char in line:
        row.append(int(char))
    grid.append(row)

result = []
for j, row in enumerate(grid):
    for i, tree in enumerate(row):

        ''' Part 1
        if tree > max(row[:i], default=-1) or tree > max(row[i+1:], default=-1)\
           or tree > max((grid[k][i] for k in range(j)), default=-1) or tree > max((grid[k][i] for k in range(len(grid[0])-1, j, -1)), default=-1):
            result.append((i, j))'''

        view = 1
        current = 0
        
        for t in row[:i][::-1]:
            current += 1
            if t >= tree:
                break
        view *= current
        current = 0

        for t in row[i+1:]:
            current += 1
            if t >= tree:
                break
        view *= current
        current = 0

        for t in list(grid[k][i] for k in range(j))[::-1]:
            current += 1
            if t >= tree:
                break
        view *= current
        current = 0

        for t in list(grid[k][i] for k in range(len(grid[0])-1, j, -1))[::-1]:
            current += 1
            if t >= tree:
                break
        view *= current

        result.append(view)

        

#print(len(set(result))) Part 1
print(max(result))

        
            
        
