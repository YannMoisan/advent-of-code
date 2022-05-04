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
curl https://adventofcode.com/2015/day/2/input -o /Users/yannmoisan/projects/perso/advent-of-code/src/main/resources/input2 --cookie "session=SESSION"
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

## 2020

### Day 7

Parsing + BFS/DFS 

### Day 10

P2 is a dynamic programming problem with the following property. Consider a voltage N. If we assume we know the number of ways to reach N-1, N-2, and N-3, then the number of ways to reach N is simply #(N-1) + #(N-2) + #(N-3).

### Day 11

Iterator.unfold to avoid while loops

### Day 12

P2 requires some basic trig operations

I do not perform well because
- I tried to reinvent : https://en.wikipedia.org/wiki/Rotation_matrix
- I didn't know that math.cos use radians and not degrees
- I messed up with clockwise/anticlockwise due to non-standard axis (y directed down)

### Day 13

TODO use https://en.wikipedia.org/wiki/Chinese_remainder_theorem

### Day 14

bit manipulation

TIL Scala doesn't have a built-in way to parse binary number strings above 32 bits. => use `java.lang.Long.parseLong`

TIL : no built-ins to replace a char on a String

TIL : no built-in leftPad

TODO don't work on Strings

### Day 18

https://en.wikipedia.org/wiki/Shunting-yard_algorithm

### Day 19

> Both parts solved by converting the rules into regex. The regex was optimized by 1) use non-capturing groups only and use them only when necessary 2) common prefix lifted from or clauses (i.e. aa|ab => a(?:a|b)).
>
> For part 2, the modified rules reduce into checking if each message matches the format ^rule42{x}rule31{y}$ where x>0; y>0; x>y.

> Runs in 15ms for Part2 and is entirely Regex based. Rule8 is just Rule42+ and Rule11 is Rule42{k}Rule31{k} where k >= 1, the latter of which is easily solvable with .NET balancing groups.

### Day 23

Circular buffer
LinkedList (O(1) removal and insertion)

> This solution does not involve any HashMap, Deque, or LinkedList of any kind. It's all done with a simple int[].
  
  Like most of you, my original solution for part 1 used a LinkedList. I knew part 2 was going to throw us a curve ball that rendered this solution inefficient and useless, but hey I'm a glutton so I did it anyway.
  
  When I got to part 2, I used a CircleNode class I made for a puzzle last year. A CircleNode encapsulates a value, it's preceding CircleNode and it's next CircleNode. After whipping that up, I was able to produce an answer in ~1.5 seconds.
  
  After a little more tinkering, I realized that we don't really care about the value that precedes our nodes, so there was no reason to update those and then at this point I just need to map 1 integer (value) to another (it's successor). What's better for mapping an int to an int than a primitive int[]?
  
  The index of the array is the value of our node and it's element is what it maps to.
  
  Here is my algorithm for updating the array:
  
  int cup = Integer.parseInt(input.substring(0, 1));
  for (int m = 1; m <= moves; m++) {
      int a = cups[cup];
      int b = cups[a];
      int c = cups[b];
      int dest = cup - 1;
      if (dest <= 0)
          dest = cups.length - 1;
      while (dest == a || dest == b || dest == c) {
          dest--;
          if (dest <= 0)
              dest = cups.length - 1;
      }
      cups[cup] = cups[c];
      int temp = cups[dest];
      cups[dest] = a;
      cups[c] = temp;
      cup = cups[cup];
  }

### Day 24

https://www.redblobgames.com/grids/hexagons/

## 2015

### Day 7

Unsigned 16 bits operation => use Char

### Day 8

Master escaping