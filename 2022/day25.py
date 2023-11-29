total = 0
with open("day25.txt", "r") as file:
    for line in file.readlines():
        for i, char in enumerate(line[:-1][::-1]):
            total += (("=", "-", "0", "1", "2").index(char) - 2) * (5**i)

# 13
# 25
# 100
# 1==

# 2022
# 3584
# 1=11-2

# 1, 2, 1=, 1-, 10, 11, 12, 2= ... 22, 1==, 1=- ...
n = 0
while 5**n < total:
    n += 1

for i in range(n):
    total += 2 * (5**i)

part1 = ""
digits = []
b = 5
while total:
    digits.append(int(total % b))
    total //= b

for digit in digits[::-1]:
    part1 += ("=", "-", "0", "1", "2")[digit]

print(f"Part 1: {part1}")
