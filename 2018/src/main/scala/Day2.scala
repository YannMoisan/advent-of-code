object Day2 extends MultiPuzzle[Int, String] {

  override def part1: Iterator[String] => Int = { iter =>
    val lines = iter.toArray
    val countByLine = lines
      .flatMap(
        _.groupBy(identity)
          .map { case (_, s) => s.length }
          .filter { count =>
            count == 2 || count == 3
          }
          .toSet
      )
    countByLine.count(_ == 2) * countByLine.count(_ == 3)
  }

  override def part2: Iterator[String] => String = { iter =>
    val lines = iter.toArray
    (for {
      s1 <- lines
      s2 <- lines
      if (countDifferingChar(s1, s2) == 1)
    } yield removeDifferingChar(s1, s2)).head
  }

  def countDifferingChar(s1: String, s2: String): Int =
    s1.zip(s2).count { case (c1, c2) => c1 != c2 }

  def removeDifferingChar(s1: String, s2: String): String =
    s1.zip(s2).collect { case (c1, c2) if c1 == c2 => c1 }.mkString
}
