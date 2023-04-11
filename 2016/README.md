## 2016

### Day 11: Radioisotope Thermoelectric Generators

Use bit manipulation to speed-up things.

```scala
  def isValidFloor(f: Floor) = {
    var gs = 0
    var ms = 0
    f.foreach { s =>
      if (s.v == 2)
        gs += 1 << s.k
      else
        ms += 1 << s.k
    }
    gs == 0 || (~gs & ms) == 0
  }
```

### Day14: One-Time Pad

Recursive construction of a `LazyList`

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
