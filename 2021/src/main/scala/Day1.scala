object Day1 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int =
    input
      .map(_.toInt)
      .foldLeft[(Option[Int], Int)]((None, 0)) {
        case ((prev, acc), a) =>
          (Some(a), acc + (if (prev.exists(a > _)) 1 else 0))
      }._2

  override def part2(input: Iterator[String]): Int =
    input
      .map(_.toInt)
      .sliding(3)
      .foldLeft[(Option[Int], Int)]((None, 0)) {
        case ((prev, acc), a) =>
          val cur = a.sum
          (Some(cur), acc + (if (prev.exists(cur > _)) 1 else 0))
      }._2

}
