with open("day21.txt", "r") as file:
    operations = []
    numbers = {}
    for line in file.readlines():
        words = line.split()
        if len(words) == 2:
            numbers[words[0][:-1]] = int(words[1])
        else:
            operations.append((words[0][:-1], words[1], words[3], words[2]))

operations1 = operations.copy()

i = -1
while len(operations1) > 0:
    i = (i + 1) % len(operations1)
    result, arg1, arg2, op = operations1[i]
    if arg1 in numbers and arg2 in numbers:
        operations1.pop(i)
        match op:
            case "+":
                n = numbers[arg1] + numbers[arg2]
            case "-":
                n = numbers[arg1] - numbers[arg2]
            case "*":
                n = numbers[arg1] * numbers[arg2]
            case "/":
                n = numbers[arg1] / numbers[arg2]
        numbers[result] = n
        
        if result == "root":
            print(f'Part 1: {n}')
            break

for result, arg1, arg2, op in operations:
    if result == "root":
        eq1, eq2 = arg1, arg2
        break

inversions = []
target = "humn"
while target not in (eq1, eq2):
    for result, arg1, arg2, op in operations:
        if arg1 == target:
            inversions.append((arg2, op, True))
            target = result
            break

        elif arg2 == target:
            inversions.append((arg1, op, False))
            target = result
            break

current = numbers[eq1 if target == eq2 else eq2]
for arg, op, left in inversions[::-1]:
    k = numbers[arg]
    match op:
        case "+":
            current -= k
        case "-":
            if left:
                current += k
            else:
                current = k - current
        case "*":
            current /= k
        case "/":
            if left:
                current *= k
            else:
                current = k / current

print(f"Part 2: {current}")
