with open("day18.txt", "r") as file:
    cubes = set(tuple(int(n) for n in line.split(",")) for line in file.readlines())

surface_area = 6 * len(cubes)
for x1, y1, z1 in cubes:
    surface_area -= sum(1 if abs(x1 - x2) + abs(y1 - y2) + abs(z1 - z2) == 1 else 0 for x2, y2, z2 in cubes)

print(f"Part 1: {surface_area}")

def is_outside_air(x, y, z, cubes):
    seen = set()
    queue = [(x, y, z)]

    while len(queue) > 0:
        x, y, z = queue.pop(0)
        if (x, y, z) in cubes:
            continue
        if (x, y, z) in seen:
            continue
        seen.add((x, y, z))
        if len(seen) > 2000:
            return True
        queue.append((x+1, y, z))
        queue.append((x-1, y, z))
        queue.append((x, y+1, z))
        queue.append((x, y-1, z))
        queue.append((x, y, z+1))
        queue.append((x, y, z-1))

    return False

surface_area = 0
for x, y, z in cubes:
    if is_outside_air(x+1, y, z, cubes):
        surface_area += 1
    if is_outside_air(x-1, y, z, cubes):
        surface_area += 1
    if is_outside_air(x, y+1, z, cubes):
        surface_area += 1
    if is_outside_air(x, y-1, z, cubes):
        surface_area += 1
    if is_outside_air(x, y, z+1, cubes):
        surface_area += 1
    if is_outside_air(x, y, z-1, cubes):
        surface_area += 1

print(f"Part 2: {surface_area}")