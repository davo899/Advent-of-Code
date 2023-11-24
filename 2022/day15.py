sensors = []
row_offset = 5_000_000
row_size = 15_000_000
row = [True for _ in range(row_size)]

with open("day15.txt", "r") as file:
  for line in file.readlines():
    s = line.split(" ")
    sensors.append((
      int(s[2][2:-1]), int(s[3][2:-1]),
      int(s[-2][2:-1]), int(s[-1][2:])
    ))

for sx, sy, bx, by in sensors:
  dx = abs(bx - sx)
  dy = abs(by - sy)
  r = (dx + dy) - abs(2_000_000 - sy)

  for i in range(1, r + 1):
    if sx + i + row_offset < row_size:
      row[sx + i + row_offset] = False
    if sx - i + row_offset >= 0:
      row[sx - i + row_offset] = False

  if r >= 0:
    row[sx + row_offset] = False

for _, _, bx, by in sensors:
  if by == 2_000_000:
    row[bx + row_offset] = True

print(f"Part 1: {sum(0 if b else 1 for b in row)}")

ranges = []
for sx, sy, bx, by in sensors:
  dx = abs(bx - sx)
  dy = abs(by - sy)
  ranges.append((sx, sy, dx + dy))

result = (-1, -1)
for i in range(4_000_000):
  if i % 100000 == 0:
    print(i)

  excluded_ranges = []
  for sx, sy, r in ranges:
    d = r - abs(sy - i)
    if d >= 0:
      excluded_ranges.append((sx-d, sx+d))

  excluded_ranges.sort(key=lambda x: x[0])
  max_ = -1
  for start, end in excluded_ranges:
    if max_ + 1 < start:
      result = (max_ + 1, i)
      break

    if end > max_:
      max_ = end

print(result)