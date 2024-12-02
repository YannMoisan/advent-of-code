object Day2 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int =
    input
      .map(_.split(" ").map(_.toInt).toList)
      .count(isSafe)

  private def isSafe(level: List[Int]) = {
    val sorted = level.sorted
    (level == sorted || level == sorted.reverse) && level.sliding(2).forall { l =>
      val diff = math.abs(l(0) - l(1))
      diff >= 1 && diff <= 3
    }
  }

  override def part2(input: Iterator[String]): Int =
    input
      .map(_.split(" ").map(_.toInt).toList)
      .count(isSafeWithRemove)

  private def isSafeWithRemove(level: List[Int]) =
    level.indices
      .map(level.patch(_, Nil, 1))
      .exists(isSafe)

}
