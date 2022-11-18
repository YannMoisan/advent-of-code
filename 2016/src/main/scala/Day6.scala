
object Day6 extends MultiPuzzle[String, String] {
  override def part1(lines: Iterator[String]): String = part(_.head)(lines)

  override def part2(lines: Iterator[String]): String = part(_.last)(lines)

  def part(f: Seq[Char] => Char) = { lines: Iterator[String] =>
    lines
      .toList
      .transpose
      .map(orderedByFreq)
      .map(f)
      .mkString("")
  }

  def orderedByFreq[A](s: Seq[A]): Seq[A] = s.groupBy(identity).mapValues(_.size).toList.sortBy(-_._2).map(_._1)
}
