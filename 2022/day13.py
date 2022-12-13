with open("day13.txt", "r") as file:
    lines = file.readlines()


def compare(v1, v2):
    if isinstance(v1, int) and isinstance(v2, int):
        return v1 < v2
    
    if isinstance(v1, int):
        v1 = [v1]
    if isinstance(v2, int):
        v2 = [v2]
        
    for i, j in zip(v1, v2):
        if isinstance(i, list) and isinstance(j, int) and len(i) == 1:
            i = i[0]
        elif isinstance(j, list) and isinstance(i, int) and len(j) == 1:
            j = j[0]
        
        if i != j:
            return compare(i, j)

    if len(v1) != len(v2):
        return len(v1) < len(v2)


def parse(s):
    if s[0] == "[":
        c = 1
        item = ""
        items = []
        for i in range(1, len(s)):
            if s[i] == "[":
                c += 1
                
            elif s[i] == "]":
                c -= 1
                if c == 0:
                    if item != "":
                        items.append(parse(item))
                    break
                
            if s[i] == "," and c == 1:
                items.append(parse(item))
                item = ""
                continue
            
            item += s[i]

        return items

    else:
        return int(s)


#result = 0
#pair = 1

divider1 = 1
divider2 = 1

i = 0
while i < len(lines):
    left = parse(lines[i][:-1])
    i += 1
    right = parse(lines[i][:-1])
    i += 2
        
    ''' Part 1
    if compare(left, right):
        result += pair

    pair += 1'''

    if compare(left, [[2]]):
        divider1 += 1
    if compare(right, [[2]]):
        divider1 += 1

    if compare(left, [[6]]):
        divider2 += 1
    if compare(right, [[6]]):
        divider2 += 1

#print(result) Part 1
        
print(divider1 * (divider2 + 1))













