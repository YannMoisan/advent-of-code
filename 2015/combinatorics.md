# 2015

More often than never, a way to solve a given puzzle is to generate all the possible solutions and use brute force to pick the right one.

## Day 9

This puzzle is the well-known [Travelling salesman problem](https://en.wikipedia.org/wiki/Travelling_salesman_problem), which is a NP-complete problem.

Hopefully, there are only 8 items in my input, so the number of possible solutions is `8!` = 40320.

## Day 13

This puzzle looks like the day9 puzzle. Some people are seating around a circular table and the best arrangement must be found.

There are 8 people (once again), so the number of possible solutions is `8!` = 40320.

Hmm, wait …

It's a circular table all these arrangements are the same (all the people have the same neighbors) : 
- A -> B -> C -> D
- B -> C -> D -> A
- C -> D -> A -> B
- D -> A -> B -> C

We should also notice that it doesn't change the arrangement if we look at it clockwise or anticlockwise, e.g.
- A -> B -> C -> D
- A -> D -> C -> B

So the number of the possible solutions is only `7!/2` = 2520

## Day 15

We are looking for 4-permutations (order matters) which the sum is 100.

The naive approach is to generate all solutions 101^4 and filter out for the right sum.

i.e. looking at 10M possibilities to retain only 176851

```scala
  println((for {
    i <- 0 to 100
    j <- 0 to 100
    k <- 0 to 100
    l <- 0 to 100
    if i + j + k + l == 100
  } yield (i, j, k, l)).size)
```

But we can do better. It is possible to iterate only through the possible solutions

```scala
  println((for {
    i <- 0 to 100
    j <- 0 to 100 - i
    k <- 0 to 100 - (i + j)
    l = 100 - (i + j + k)
  } yield (i, j, k, l)).size)
```

# Day 17

In this puzzle, we have 20 containers, and we need to find all combinations that can contain exactly a given amount of liquid.

In other words, we need to generate the powerset. 2^20 = 1048576

# Day 21

In this puzzle, the player can wear items : weapons, armor and rings.

The best combination needs to be found.

A player 
- must wear a weapon, so choose 1 amongst 5 = 5C1 = 5
- can buy 0-2 rings, so it's choose [0-2] amongst 6 = 6C0 + 6C1 + 6C2 = 1 + 6 + 15 = 22
- armor is optional, so it's choose [0-1] amongst 5 = 5C0 + 5C1 = 1 + 5 = 6

So the number of combinations is 5 * 22 * 6 = 660

# Day 24

This puzzle looks like the day 17. The solutions to consider is the power of a set of 28 elements, so 2^28.

Hopefully, we might not need to explore all possible solutions.

We can iterate in increasing length of candidates.