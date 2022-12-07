with open("day3.txt", "r") as file:
    lines = file.readlines()


total = 0
lines = [line[:-1] for line in lines]

'''
for line in lines:
    split = len(line) // 2
    comp1 = line[split:]
    comp2 = line[:split]

    for c in comp1:
        if c in comp2:
            print(comp1, comp2, c)
            if c.isupper():
                total += 26 + (ord(c) + 1 - ord("A"))
            else:
                total += ord(c) + 1 - ord("a")
            break'''


lines = [(lines[i], lines[i+1], lines[i+2]) for i in range(0, len(lines), 3)]

for bag1, bag2, bag3 in lines:
    for c in bag1:
        if c in bag2 and c in bag3:
            print(bag1, bag2, bag3, c)
            if c.isupper():
                total += 26 + (ord(c) + 1 - ord("A"))
            else:
                total += ord(c) + 1 - ord("a")
            break

print(total)
                
