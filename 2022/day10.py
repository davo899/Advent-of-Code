with open("day10.txt", "r") as file:
    lines = file.readlines()

lines = [line[:-1].split(" ") for line in lines]

results = [
        "#", #hacky fix
        "",
        "",
        "",
        "",
        "",
        "",
    ]
row = 0
def inc_cycle(cycle, x):
    global results, row
    cycle += 1

    ''' Part 1
    if cycle % 40 == 20:
        results.append(cycle * x)'''
        
    if cycle % 40 == 0:
        row += 1

    if abs(x - (cycle % 40)) <= 1:
        results[row] += "#"
    else:
        results[row] += "."

    return cycle


cycle = 0
x = 1
for line in lines:
    if line[0] == "noop":
        cycle = inc_cycle(cycle, x)
    else:
        cycle = inc_cycle(cycle, x)
        x += int(line[1])
        cycle = inc_cycle(cycle, x)

#print(sum(results)) Part 1
        
for r in results:
    print(r)
