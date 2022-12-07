with open("day4.txt", "r") as file:
    lines = file.readlines()

lines = [line.split(",") for line in lines]
lines = [(line[0].split("-"), line[1].split("-")) for line in lines]
lines = [(
    (int(line[0][0]), int(line[0][1])),
    (int(line[1][0]), int(line[1][1]))
) for line in lines]

result = 0

''' Part 1
for line in lines:
    if (line[0][0] <= line[1][0] and line[1][1] <= line[0][1]):
        result += 1
    elif (line[1][0] <= line[0][0] and line[0][1] <= line[1][1]):
        result += 1 '''

for line in lines:
    if (line[0][0] <= line[1][0] <= line[0][1]):
        result += 1
    elif (line[0][0] <= line[1][1] <= line[0][1]):
        result += 1
    elif (line[1][0] <= line[0][0] <= line[1][1]):
        result += 1
    elif (line[1][0] <= line[0][1] <= line[1][1]):
        result += 1

print (result)
