monkeys = [
    {
        "items": [50, 70, 89, 75, 66, 66],
        "operation": lambda x: x * 5,
        "test": 2,
        "throw": (2, 1)
    },
    {
        "items": [85],
        "operation": lambda x: x * x,
        "test": 7,
        "throw": (3, 6)
    },
    {
        "items": [66, 51, 71, 76, 58, 55, 58, 60],
        "operation": lambda x: x + 1,
        "test": 13,
        "throw": (1, 3)
    },
    {
        "items": [79, 52, 55, 51],
        "operation": lambda x: x + 6,
        "test": 3,
        "throw": (6, 4)
    },
    {
        "items": [69, 92],
        "operation": lambda x: x * 17,
        "test": 19,
        "throw": (7, 5)
    },
    {
        "items": [71, 76, 73, 98, 67, 79, 99],
        "operation": lambda x: x + 8,
        "test": 5,
        "throw": (0, 2)
    },
    {
        "items": [82, 76, 69, 69, 57],
        "operation": lambda x: x + 7,
        "test": 11,
        "throw": (7, 4)
    },
    {
        "items": [65, 79, 86],
        "operation": lambda x: x + 5,
        "test": 17,
        "throw": (5, 0)
    },
]

inspections = [0, 0, 0, 0, 0, 0, 0, 0]
#for _ in range(20): Part 1
for _ in range(10000):
    for i, monkey in enumerate(monkeys):
        inspections[i] += len(monkey["items"])
        
        #monkey["items"] = list(map(lambda x: x // 3, map(monkey["operation"], monkey["items"]))) Part 1
        monkey["items"] = list(map(lambda x: x % (2 * 7 * 13 * 3 * 19 * 5 * 11 * 17), map(monkey["operation"], monkey["items"])))

        passes = list(filter(lambda x: x % monkey["test"] == 0, monkey["items"]))
        monkeys[monkey["throw"][0]]["items"] += passes
        monkeys[monkey["throw"][1]]["items"] += list(filter(lambda x: x not in passes, monkey["items"]))
        monkeys[i]["items"] = []

inspections.sort()
print(inspections[-1] * inspections[-2])
