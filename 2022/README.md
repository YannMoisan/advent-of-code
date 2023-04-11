kl## Day 1: Calorie Counting

## Day 2: Rock Paper Scissors

## Day 3: Rucksack Reorganization

## Day 4: Camp Cleanup

## Day 5: Supply Stacks

usage of mutable.Stack

## Day 6: Tuning Trouble

## Day 7: No Space Left On Device

## Day 8: Treetop Tree House

Grid

## Day 9: Rope Bridge

Refactor with position and direction

## Day 10: Cathode-Ray Tube

## Day 11: Monkey in the Middle

## Day 12: Hill Climbing Algorithm

Shortest path in unweighted graph

## Day 13: Distress Signal

recursive computation and parsing

## Day 14: Regolith Reservoir

space filling with sand

## Day 15: Beacon Exclusion Zone

Interval

## Day 16: Proboscidea Volcanium

`Valve AA has flow rate=0; tunnels lead to valves DD, II, BB`

I had to improve BFS implementation for part 1:
- use an Iterator instead of a Stream to reduce mem usage
- do not compute paths (there are not needed)

part2 is still too long to finish …

- Use Floyd Warshall to compute all distances
- Only consider move followed by open
- Greedy approach doesn't work for part2 (i.e. running the part1 for me and after for the elephant doesn't give the right answer)

[day=16] (t=169ms) 1947
[day=16] (t=35692ms) 2556

## Day 17: Pyroclastic Flow

Tetris simulation

## Day 18: Boiling Boulders

3D flood fill

## Day 19: Not Enough Minerals

```
Blueprint 1:
  Each ore robot costs 4 ore.
  Each clay robot costs 2 ore.
  Each obsidian robot costs 3 ore and 14 clay.
  Each geode robot costs 2 ore and 7 obsidian.
```

For blueprint1, without any optimization, there are #next=13,9M calls to next in BFS.
                                                    #next=10,0M

It looks like day16

https://old.reddit.com/r/adventofcode/comments/zpihwi/2022_day_19_solutions/j0vvtdt/

```
- Pruning a branch if its best possible score (crafting one geode robot per minute until the end) is worse than the current best branch (and trying to perform the temptatively best actions first to reach the best branch ASAP).
- Building a geode robot immediately whenever they are available.
- Ending a branch early if there is no possible way to generate enough obsidian to craft any more geode robots.
- Forbidding the crafting of robots of a type if we are already generating the maximum needed of that material per minute for any recipe.
- Pruning a branch if it tries to create a robot of a certain type, when it could have been created in the previous state too but it decided to do nothing instead. This one saves quite a lot of time.
```

The trick is to aggressively prune the search space.

## Day20: Grove Positioning System

I've lost A LOT OF time because I didn't notice duplicates in the input.

## Day 21: Monkey Math

recursive computation of a tree of operations

part2 : simple example to help reasoning

```
25 = 5 + ( 10 * (3 - h))
20 = 10 * (3 - h)
2  = 3 - h
1  = h
```

part2, cf: https://www.reddit.com/r/adventofcode/comments/zrav4h/comment/j1bf4cq/?utm_source=share&utm_medium=web2x&context=3

```
private def solve(lhs: Expression, rhs: Expression): Long =
  lhs match
    case Add(x, y)      if containsHuman(x) => solve(x, Subtract(rhs, y))
    case Add(x, y)      if containsHuman(y) => solve(y, Subtract(rhs, x))
    case Subtract(x, y) if containsHuman(x) => solve(x, Add(rhs, y))
    case Subtract(x, y) if containsHuman(y) => solve(y, Subtract(x, rhs))
    case Divide(x, y)   if containsHuman(x) => solve(x, Multiply(rhs, y))
    case Divide(x, y)   if containsHuman(y) => solve(y, Divide(x, rhs))
    case Multiply(x, y) if containsHuman(x) => solve(x, Divide(rhs, y))
    case Multiply(x, y) if containsHuman(y) => solve(y, Divide(rhs, x))
    case _ => evaluate(rhs)
```

## Day 22: Monkey Map

My cube doesn't have the same shape as the one in the example

## Day 23: Unstable Diffusion

Yet another Conway game of life with complicated simulation rules

use a Set instead of a Grid because it's infinite

## Day 24: Blizzard Basin

Nice variation on the maze problem.

The solution uses a BFS with a state containing the current position and the time, so we can avoid visiting the same state twice.
Note: the current position is not sufficient due to modification of external conditions (aka blizzards)

## Day 25: Full of Hot Air

Arithmetic computation of numbers `1=-1=` in base 5.