# Goal

The goal is to allow you to write as little code as possible for each puzzle.

# Project structure

I recommend using a [multi project](https://www.scala-sbt.org/1.x/docs/Multi-Project.html) build.

```
.
├── 2021
├── build.sbt
└── core
```

# Howto

A generic application `Puzzles` is provided and is responsible for
- finding and creating the requested day.
- reading the input file.
- calling methods for each part and collecting execution time.

The implementation relies on the _convention over configuration_ principle.

The input files must be placed in `src/main/resources` and suffixed by the day `input{{day}}`.

The code must be placed into an object suffixed with the day, such as `Day{{day}}.scala` 

```
2021
├── src
│  ├── main
│  │  ├── resources
│  │  │  ├── input1
│  │  │  └── input2
│  │  └── scala
│  │     ├── Day1.scala
│  │     └── Day2.scala
```

Two base traits are provided
- `SinglePuzzle` if the input file contains one line
- `MultiPuzzle` if the input file contains multiple lines

Even though the answer must be filled in a text field, the answers are typed.
Both traits have 2 type parameters, one for the output of part1, and one for the output of part2.

# Testing

A helper class `AllPuzzlesSpec`, based on [table driven property checks](https://www.scalatest.org/user_guide/table_driven_property_checks), is provided to check for non-regression once a puzzle is solved.
It allows performing some refactoring safely afterward.

# Show me the code

## main

```scala
object Day1 extends SinglePuzzle[Int, Int] {
  override def part1(input: String): Int = input.length
  override def part2(input: String): Int = input.length
}
```

## test

```scala
class NonRegressionSpec extends AllPuzzlesSpec(
  Table(
    ("day", "part", "result"),
    (1, 1, 42),
    (1, 2, 43),
    (2, 1, 44),
    (2, 2, 45),
  )
)
```

# Useful commands

Run all days

```
advent-of-code-2020/runMain Puzzles
```

```
[day=1] (t=2ms) 42
[day=1] (t=1ms) 43
[day=2] (t=1ms) 44
[day=2] (t=2ms) 45
...
```

Run a single day

```
advent-of-code-2020/runMain Puzzles 1
```

Run non-regression tests

```
advent-of-code-2020/test
```
