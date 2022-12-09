with open("day9.txt", "r") as file:
    lines = file.readlines()

lines = [line[:-1].split(" ") for line in lines]
lines = [(line[0], int(line[1])) for line in lines]

tail_visits = []
head = [0, 0]

#Part 1
tail = [[0, 0]]

tail = [
	[0, 0], 
	[0, 0],
	[0, 0],
	[0, 0],
	[0, 0],
	[0, 0],
	[0, 0],
	[0, 0],
	[0, 0],
]
tail_visits.append(tuple(tail[-1]))
for direction, moves in lines:
    for _ in range(moves):
        
        if direction == "U":
            head[1] -= 1
        elif direction == "D":
            head[1] += 1
        elif direction == "L":
            head[0] -= 1
        elif direction == "R":
            head[0] += 1

        h = head
        for i, t in enumerate(tail):
            xdif = h[0] - t[0]
            ydif = h[1] - t[1]

            if xdif > 1:
	            t[0] += 1
	            if ydif == 1:
		            t[1] += 1
	            if ydif == -1:
		            t[1] -= 1

            if xdif < -1:
	            t[0] -= 1
	            if ydif == 1:
		            t[1] += 1
	            if ydif == -1:
		            t[1] -= 1

            if ydif > 1:
	            t[1] += 1
	            if xdif == 1:
		            t[0] += 1
	            if xdif == -1:
		            t[0] -= 1

            if ydif < -1:
	            t[1] -= 1
	            if xdif == 1:
		            t[0] += 1
	            if xdif == -1:
		            t[0] -= 1
		            
            h = t
        
        tail_visits.append(tuple(tail[-1]))

print(len(set(tail_visits)))
