score = 0

with open("day2.txt", "r") as file:
    lines = file.readlines()

for line in lines:

    if line[0] == "A":
        if line[2] == "X":
            score += 0 + 3
        if line[2] == "Y":
            score += 3 + 1
        if line[2] == "Z":
            score += 6 + 2
            
    if line[0] == "B":
        if line[2] == "X":
            score += 0 + 1
        if line[2] == "Y":
            score += 3 + 2
        if line[2] == "Z":
            score += 6 + 3
            
    if line[0] == "C":
        if line[2] == "X":
            score += 0 + 2
        if line[2] == "Y":
            score += 3 + 3
        if line[2] == "Z":
            score += 6 + 1

print(score)
