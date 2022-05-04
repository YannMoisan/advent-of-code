# Day 1: Sonar Sweep

# Day 2: Dive!

# Day 3: Binary Diagnostic (binary-string)

- I've had hard times to spot a bug a count >= (length/2) when length is odd (3 >= 7/2)
  => takeaway : baby step, use provided input
- It exists a method to parse binary string `Integer.parseInt(s, 2)`

# Day 4: Giant Squid (2d-grid)

- set in the state
- immutable ?

design a 2d grid class

# Day 5: Hydrothermal Venture (geometry)

- HOF to share the code between both parts
- rework geometry calculation

# Day 6: Lanternfish

- brute force doesn't work

# Day 7: The Treachery of Whales

use the median ?

# Day 8: Seven Segment Search

# Day 9: Smoke Basin (2d-grid, graph)

- classical error

```scala
scala> val s = "12"
val s: String = 12

scala> s(0) + s(1)
val res5: Int = 99
```

- flood fill

# Day 10: Syntax Scoring

use a stack

# Day 11: Dumbo Octopus (2D Grid)

# Day 12: Passage Pathing (Graph)

BFS / DFS

# Day 13: Transparent Origami

# Day 14: Extended Polymerization

# Day 15: Chiton (dijkstra)

shortest path in undirected weighted graph -> dijkstra

# Day 16: Packet Decoder

BigInt(hexa, 16).toString(2)

# Day 17: Trick Shot

Iterator.iterate(init)(next).takeWhile().find()
How to avoid brute force ?

# Day 19
number of beacons per scanner is variable
https://www.euclideanspace.com/maths/discrete/groups/categorise/finite/cube/index.htm

# Day20

Array2D in scala

```
Array.tabulate(2,3)((x,y) => x*3 + y)
```

```
012
345
```

# Day 21

https://www.reddit.com/r/adventofcode/comments/rl6p8y/comment/hpe8pmy

```python
pos1, pos2 = [int(x.split()[-1]) for x in open(0)]

def play1(pos1, pos2, score1=0, score2=0, i=0):
    if score2 >= 1000: return i*score1
    pos1 = (pos1 + 3*i+6) % 10 or 10
    return play1(pos2, pos1, score2, score1+pos1, i+3)

from functools import cache
@cache
def play2(pos1, pos2, score1=0, score2=0):
    if score2 >= 21: return 0, 1

    wins1, wins2 = 0, 0
    for move, n in (3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1):
        pos1_ = (pos1 + move) % 10 or 10
        w2, w1 = play2(pos2, pos1_, score2, score1 + pos1_)
        wins1, wins2 = wins1 + n*w1, wins2 + n*w2
    return wins1, wins2

print(play1(pos1, pos2), max(play2(pos1, pos2)))
```


# Day22

https://www.reddit.com/r/adventofcode/comments/rlxhmg/comment/hpizza8/?utm_source=share&utm_medium=web2x&context=3


```python3
import re

input = re.findall(r'(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)', open('input.txt').read())
cubes = []

for step in input:
    step = [int(x) if x.strip('-').isnumeric() else x for x in step]
    [op, ux, vx, uy, vy, uz, vz] = step
    for cubes_i in range(len(cubes)):
        [ux2, vx2, uy2, vy2, uz2, vz2] = cubes[cubes_i]
        if ux > vx2 or vx < ux2 or uy > vy2 or vy < uy2 or uz > vz2 or vz < uz2: # new on zone not overlapping existing on zone
            continue
        cubes[cubes_i] = None
        if ux > ux2:
            cubes.append((ux2, ux - 1, uy2, vy2, uz2, vz2))
        if vx < vx2:
            cubes.append((vx + 1, vx2, uy2, vy2, uz2, vz2))
        if uy > uy2:
            cubes.append((max(ux2, ux), min(vx2, vx), uy2, uy - 1, uz2, vz2))
        if vy < vy2:
            cubes.append((max(ux2, ux), min(vx2, vx), vy + 1, vy2, uz2, vz2))
        if uz > uz2:
            cubes.append((max(ux2, ux), min(vx2, vx), max(uy2, uy), min(vy2, vy), uz2, uz - 1))
        if vz < vz2:
            cubes.append((max(ux2, ux), min(vx2, vx), max(uy2, uy), min(vy2, vy), vz + 1, vz2))
    if op == 'on':
        cubes.append((min(ux, vx), max(ux, vx), min(uy, vy), max(uy, vy), min(uz, vz), max(uz, vz)))
    cubes = [cube for cube in cubes if cube is not None]

on_count = 0
for cube in cubes:
    [ux, vx, uy, vy, uz, vz] = cube
    on_count += (vx - ux + 1) * (vy - uy + 1) * (vz - uz + 1)
print(on_count)
```

# Day23

-bfs-
I resolved the part1 manually with a pen and a paper.
I failed to solve the part2 manually, so I had to write code.

# Day25

- fixpoint
- 2D grid