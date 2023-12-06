object Day6 extends MultiPuzzle[Int, Int] {
  def count(time: Int, distance: Int): Int =
    (1 to time).count(start => start * (time - start) > distance)

  override def part1(input: Iterator[String]): Int =
    count(58, 434) *
      count(81, 1041) *
      count(96, 2219) *
      count(76, 1218)

  override def part2(input: Iterator[String]): Int = {
    val first = (1 to 58819676).find { start =>
      start.toLong * (58819676 - start).toLong > 434104122191218L
    }
    58819676 - first.get * 2 + 1
  }
}
