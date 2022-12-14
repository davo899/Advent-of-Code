with open("day14.txt", "r") as file:
    lines = file.readlines()

lines = [line[:-1].split(" -> ") for line in lines]

maxY = 0
grid = []
for i in range(1000):
    row = []
    for j in range(1000):
        row.append(".")
    grid.append(row)

for line in lines:
    prevX = None
    prevY = None
    
    for pointstr in line:
        x, y = tuple(map(int, pointstr.split(",")))
        
        if y > maxY:
            maxY = y
            
        if prevX != None:
            if x == prevX:
                start = min(y, prevY)
                for i in range(abs(y - prevY) + 1):
                    grid[start + i][x] = "#"
            else:
                start = min(x, prevX)
                for i in range(abs(x - prevX) + 1):
                    grid[y][start + i] = "#"
                    
        prevX, prevY = (x, y)

grid[maxY + 2] = ["#" for _ in range(1000)]
        

result = 0
#while y < len(grid) - 1: Part 1
end = False
while not end:
    x, y = (500, 0)

    while y < len(grid) - 1:
        if grid[y + 1][x] == ".":
            y += 1
        elif grid[y + 1][x - 1] == ".":
            x -= 1
        elif grid[y + 1][x + 1] == ".":
            x += 1
        else:
            grid[y][x] = "o"
            if (x, y) == (500, 0):
                end = True
            break
    
    result += 1

#print(result - 1) Part 1
print(result)
