import itertools
valves = []
pressure_valves = []
graph = {}

with open("day16.txt", "r") as file:
  for line in file.readlines():
    s = line.split(" ")
    valves.append((
      s[1],
      int(s[4][5:-1]),
      [v[:-1] for v in s[9:]]
    ))

def max_pressure(current_valve, target_valves, graph, minutes_left, total_pressure, opened):
    if minutes_left <= 0:
        return total_pressure

    next_minutes_left = minutes_left - 1
    next_pressure = total_pressure + graph[current_valve]["rate"] * next_minutes_left

    if len(target_valves) == len(opened) + 1:
        return next_pressure
  
    return max(max_pressure(
        next_valve,
        target_valves,
        graph,
        next_minutes_left - graph[current_valve]["adjacents"][next_valve],
        next_pressure,
        opened + [current_valve]
    ) for next_valve, _ in target_valves if (current_valve != next_valve and next_valve not in opened))
    
def valve_distance(start, end, valves, visited):
  if start == end:
    return 0
  
  for valve, _, adjacent_valves in valves:
    if valve == start:
        dists = [1 + valve_distance(next_valve, end, valves, visited + [valve]) for next_valve in adjacent_valves if next_valve not in visited]

        if len(dists) == 0:
            return 99999999999999

        return min(dists)
    
for valve, rate, _ in valves:
  if rate > 0:
    pressure_valves.append((valve, rate))

for start, rate in pressure_valves:
    adjacents = {}
    for end, _ in pressure_valves:
        if start != end:
            adjacents[end] = valve_distance(start, end, valves, [])

    graph[start] = {
        "adjacents": adjacents,
        "rate": rate
    }

print(f'Part 1: {max(max_pressure(next_valve, pressure_valves, graph, 30 - valve_distance("AA", next_valve, valves, []), 0, []) for next_valve, _ in pressure_valves)}')

result = -1
for combination in itertools.combinations(pressure_valves, len(pressure_valves) // 2):
    complement = tuple(valve for valve in pressure_valves if valve not in combination)
    elf = max(max_pressure(next_valve, combination, graph, 26 - valve_distance("AA", next_valve, valves, []), 0, []) for next_valve, _ in combination)
    elephant = max(max_pressure(next_valve, complement, graph, 26 - valve_distance("AA", next_valve, valves, []), 0, []) for next_valve, _ in complement)
    total = elf + elephant
    if total > result:
        result = total

print(f"Part 2: {result}")
