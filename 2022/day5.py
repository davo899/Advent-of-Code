'''
                [B]     [L]     [S]
        [Q] [J] [C]     [W]     [F]
    [F] [T] [B] [D]     [P]     [P]
    [S] [J] [Z] [T]     [B] [C] [H]
    [L] [H] [H] [Z] [G] [Z] [G] [R]
[R] [H] [D] [R] [F] [C] [V] [Q] [T]
[C] [J] [M] [G] [P] [H] [N] [J] [D]
[H] [B] [R] [S] [R] [T] [S] [R] [L]
 1   2   3   4   5   6   7   8   9 
'''

stacks = [
    ["H", "C", "R"],
    ["B", "J", "H", "L", "S", "F"],
    ["R", "M", "D", "H", "J", "T", "Q"],
    ["S", "G", "R", "H", "Z", "B", "J"],
    ["R", "P", "F", "Z", "T", "D", "C", "B"],
    ["T", "H", "C", "G"],
    ["S", "N", "V", "Z", "B", "P", "W", "L"],
    ["R", "J", "Q", "G", "C"],
    ["L", "D", "T", "R", "H", "P", "F", "S"]
]

stacks = [stack[::-1] for stack in stacks]

with open("day5.txt", "r") as file:
    lines = file.readlines()

lines = [line.split(" ") for line in lines]
lines = [(int(line[1]), int(line[3]), int(line[5])) for line in lines]

for n, s1, s2 in lines:
    s1 -= 1
    s2 -= 1
    #stacks[s2] = (stacks[s1][:n])[::-1] + stacks[s2] #Part 1
    stacks[s2] = stacks[s1][:n] + stacks[s2]          #Part 2
    stacks[s1] = stacks[s1][n:]


for stack in stacks:
    print(stack)
