from collections import deque
from math import prod
blueprints = []

with open("day19.txt", "r") as file:
    for line in file.readlines():
        words = line.split()
        ore_robot_cost = int(words[6])
        clay_robot_cost = int(words[12])
        obsidian_robot_cost = (int(words[18]), int(words[21]))
        geode_robot_cost = (int(words[27]), int(words[30]))

        blueprints.append({
            "ore_robot_cost": ore_robot_cost,
            "clay_robot_cost": clay_robot_cost,
            "obsidian_robot_cost": obsidian_robot_cost,
            "geode_robot_cost": geode_robot_cost,
        })

def max_geodes(blueprint, start_time):
    ore_robot_cost = blueprint["ore_robot_cost"]
    clay_robot_cost = blueprint["clay_robot_cost"]
    obsidian_robot_ore_cost, obsidian_robot_clay_cost = blueprint["obsidian_robot_cost"]
    geode_robot_ore_cost, geode_robot_obsidian_cost = blueprint["geode_robot_cost"]
    ore_limit = max([ore_robot_cost, clay_robot_cost, obsidian_robot_ore_cost, geode_robot_ore_cost])

    max_ = -1
    seen = set()
    queue = deque([(1, 0, 0, 0, 0, 0, 0, 0, start_time)])
    while len(queue) > 0:
        state = queue.popleft()

        ore_robots, clay_robots, obsidian_robots, geode_robots, ore, clay, obsidian, geodes, time = state
        
        max_ = max(max_, geodes)
        if time == 0:
            continue
        
        if ore > time * ore_limit:
            ore = time * ore_limit
        if clay > (r := time * obsidian_robot_clay_cost):
            clay = r
        if obsidian > (r := time * geode_robot_obsidian_cost):
            obsidian = r

        if ore_robots > ore_limit:
            ore_robots = ore_limit
        if clay_robots > obsidian_robot_clay_cost:
            clay_robots = obsidian_robot_clay_cost
        if obsidian_robots > geode_robot_obsidian_cost:
            obsidian_robots = geode_robot_obsidian_cost
        
        state = (ore_robots, clay_robots, obsidian_robots, geode_robots, ore, clay, obsidian, geodes, time)
        if state in seen:
            continue
        seen.add(state)

        queue.append((ore_robots, clay_robots, obsidian_robots, geode_robots, ore + ore_robots, clay + clay_robots, obsidian + obsidian_robots, geodes + geode_robots, time - 1))
        if ore >= ore_robot_cost:
            queue.append((ore_robots + 1, clay_robots, obsidian_robots, geode_robots, ore + ore_robots - ore_robot_cost, clay + clay_robots, obsidian + obsidian_robots, geodes + geode_robots, time - 1))
        if ore >= clay_robot_cost:
            queue.append((ore_robots, clay_robots + 1, obsidian_robots, geode_robots, ore + ore_robots - clay_robot_cost, clay + clay_robots, obsidian + obsidian_robots, geodes + geode_robots, time - 1))
        if ore >= obsidian_robot_ore_cost and clay >= obsidian_robot_clay_cost:
            queue.append((ore_robots, clay_robots, obsidian_robots + 1, geode_robots, ore + ore_robots - obsidian_robot_ore_cost, clay + clay_robots - obsidian_robot_clay_cost, obsidian + obsidian_robots, geodes + geode_robots, time - 1))
        if ore >= geode_robot_ore_cost and obsidian >= geode_robot_obsidian_cost:
            queue.appendleft((ore_robots, clay_robots, obsidian_robots, geode_robots + 1, ore + ore_robots - geode_robot_ore_cost, clay + clay_robots, obsidian + obsidian_robots - geode_robot_obsidian_cost, geodes + geode_robots, time - 1))

    return max_

print(f"Part 1: {sum(max_geodes(blueprint, 24) * (i + 1) for i, blueprint in enumerate(blueprints))}")
print(f"Part 2: {prod(max_geodes(blueprint, 32) for blueprint in blueprints[:3])}")