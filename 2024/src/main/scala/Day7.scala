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
