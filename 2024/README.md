It's been 10 years since I started enjoying [Advent of Code](https://adventofcode.com/2024), 
a series of daily coding exercises in December. 
Over this time, I've learned a lot, and I firmly believe that practice makes you a better developer. 
Since the time required to solve these exercises is still manageable, 
I’d like to share a wrap-up of the first week—mostly because I probably won’t have time to do it later.

# Day1

This was a perfect opportunity to use zip and unzip.

```scala
scala> List((1,2), (3,4), (5,6)).unzip
val res0: (List[Int], List[Int]) = (List(1, 3, 5),List(2, 4, 6))

scala> pair._1.zip(pair._2)
val res2: List[(Int, Int)] = List((1,2), (3,4), (5,6))
```

With these built-in methods, there’s no need to loop explicitly over collections, and the intent is clearer.

Similarly, to count occurrences in a list, verbose code like this:

```scala
scala> List(1,2,1,1,2).groupBy(identity).view.mapValues(_.length).toMap
val res1: scala.collection.immutable.Map[Int,Int] = Map(1 -> 3, 2 -> 2)
```

can be simplified using `groupMapReduce`:

```scala
scala> List(1,2,1,1,2).groupMapReduce(identity)(_=>1)(_+_)
val res4: scala.collection.immutable.Map[Int,Int] = Map(1 -> 3, 2 -> 2)
```

# Day2

Once again, sliding windows proved useful for grouping elements in pairs, 
making it easy to handle iteration logic involving the current element and its predecessor.

```scala
scala> List(1,2,3,4).sliding(2).foreach(println)
List(1, 2)
List(2, 3)
List(3, 4)
```

Another handy method was patch, which isn’t very intuitive but works well for removing elements from a collection.

```scala
scala> List(1,2,3,4).patch(2, Nil, 1)
val res6: List[Int] = List(1, 2, 4)
```

# Day3

This day was a tough reminder that I’m still too slow with regular expressions 
(I never seem to remember how escaping works with triple-quote syntax in Scala). 
I also had to refresh my memory on the non-greedy syntax .*?.

# Day5

String interpolation continues to be a convenient way to destructure strings:

```scala
case s"${fst}|${snd}" => (fst.toInt, snd.toInt)
```

I wasted a lot of time due to a silly mistake: using i instead of arr(i). Since both are integers, the compiler didn’t catch it.

For part two, I initially reimplemented a bubble sort before realizing I could just use the built-in sortWith. 
Ultimately, the task boiled down to writing code to compare two elements:

```scala
private def lt(orderingRules: Map[Int, Set[Int]]): (Int, Int) => Boolean = { (a, b) =>
  orderingRules.get(a).exists(_.contains(b))
}
```

# Day 6

Over the years, I’ve extracted reusable code for Advent of Code. 
This time, I reused a grid implementation, which was helpful. 
However, the design wasn’t great, and I lost some time figuring out how it worked. 

The upside? It worked on the first try, avoiding common coding mistakes like off-by-one errors.

# Day7

This was the day where a 32-bit integer overflow occurred. Fortunately, it happened during parsing, and the error message made it clear:

```scala
[error] java.lang.NumberFormatException: For input string: "634495747544"
```

I also reused a combinatorics method that I had extracted in previous years. 
Honestly, it’s satisfying to see how clear and straightforward the code can be in Scala:

```
object Day7 extends MultiPuzzle[Long, Long] {
  val opsPart1: List[(Long, Long) => Long] = List(_ + _, _ * _)
  val opsPart2: List[(Long, Long) => Long] = List(_ + _, _ * _, (a, b) => s"$a$b".toLong)

  override def part1(input: Iterator[String]): Long = common(input, opsPart1)

  override def part2(input: Iterator[String]): Long = common(input, opsPart2)

  private def common(input: Iterator[String], ops: List[(Long, Long) => Long]) =
    input.map { s =>
      val numbers    = s.split("[: ]+").map(_.toLong)
      val candidates = Combinatorics.permutationsWithReplacement(ops, numbers.length - 2)
      val equationCouldBeTrue = candidates.exists { candidate =>
        val (expected :: fst :: tail) = numbers.toList
        val actual                    = tail.zip(candidate).foldLeft(fst) { case (acc, (cur, op)) => op(acc, cur) }
        actual == expected
      }
      if (equationCouldBeTrue) numbers.head else 0
    }.sum
}
```

If you're interested, you can find [all my solutions on Github](https://github.com/yannmoisan/advent-of-code/).

# Day 12

## Part 2

> The real trick: the number of sides is equal to the number of corners! This makes sense -- each corner is a turn that starts a new side, so they have to be one-to-one. So count the corners!