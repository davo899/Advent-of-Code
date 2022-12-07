with open("day6.txt", "r") as file:
    line = file.readlines()[0]

result = 0
stack = []
for i, char in enumerate(line):
    print(char, stack)
    if char in stack:
        while stack[0] != char:
            stack = stack[1:]
        stack = stack[1:]
        
    stack.append(char)
    if len(stack) == 14:
        print(i)
        break

    
