# Advent of code

## Useful commands

Run all days

```
advent-of-code-2020/runMain PuzzlesApp
```

Run a single day

```
advent-of-code-2020/runMain PuzzlesApp 1
```

## Others

```
curl https://adventofcode.com/2015/day/2/input -o /Users/yann.moisan/projects/perso/advent-of-code/src/main/resources/input2 --cookie "session=SESSION"
```

## TODO

- [ ] configure travis CI
- [ ] scapegoat
- [ ] test arguments -oDF ?
- [X] Puzzles should take the day to run as parameters
- [X] Puzzles should time the execution
- [X] Puzzles should have a mode to run all tests
- [X] Puzzles part1 should not return a function

## 2019

### Day 6

- common prefix into two list
- no mutable acc
- BFS instead of DFS
- tree traversal with cats

## 2016

### Liked

- Enjoy coding some fun things each day
- Repetitions of some techniques through different puzzles, such as parsing, md5, looping over a A => A, breadth-first search.
So, you feel at ease to implement this kind of things.
- Go back to code and refactor to extract reusable code (thanks to Travis CI for non reg tests)

### Learned

- To leverage all the learning potential : not only solve the puzzle but try to come up with the smartest implementation
- Improve functional programming skills by using pure functions, lazy data structure like `Stream`, …
- Take the opportunity to try a new FP framework : [cats](http://typelevel.org/cats/), cf code for Day1

### Lacked

- I would have liked to solve more varied puzzles. For example, a dynamic programming puzzle.
