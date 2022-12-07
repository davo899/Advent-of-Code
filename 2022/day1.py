with open("day1.txt", "r") as file:
    lines = file.readlines()

bags = []
current = 0
for line in lines:
    if line == "\n":
        bags.append(current)
        current = 0
    else:
        current += int(line)

# print(max(bags)) Part 1

bags.sort()
print(sum(bags[-3:]))
