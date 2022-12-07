with open("day7.txt", "r") as file:
    lines = file.readlines()

lines = [(line[:-1]).split(" ") for line in lines]
result = 0
current_size = 0
candidates = []

def dir_size(name, dire):
    size = sum(value if isinstance(value, int) else dir_size(key, value) for key, value in dire.items())
    
    #global result      Part 1
    #if size <= 100000:
    #    result += size
    global candidates
    global current_size
    if current_size != 0 and current_size - size <= 40000000:
        candidates.append(size)
        
    return size

def current_dir(index, filesys):
    current = filesys
    for i in index:
        current = current[i]

    return current

filesys = {"/":{}}
index = []

i = 0
while i < len(lines):
    line = lines[i]
    
    if line[0] == "$":
        if line[1] == "cd":
            if line[2] == "..":
                index.pop()
            else:
                index.append(line[2])
            i += 1
                
        elif line[1] == "ls":
            dire = current_dir(index, filesys)
            i += 1
            while i < len(lines) and (current := lines[i])[0] != "$":
                if current[0] == "dir":
                    dire[current[1]] = {}
                else:
                    dire[current[1]] = int(current[0])
                i += 1

current_size = dir_size("", filesys)
print(current_size)
dir_size("", filesys)
print(min(candidates))
