object Day3 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int =
    input.map(s => common(s).map(value).sum).sum

  override def part2(input: Iterator[String]): Int = {
    val grouped: List[List[String]] = input.toList.grouped(3).toList
    grouped.map(grp => value(grp.map(_.toSet).reduce(_ intersect _).head)).sum
  }

  private def common(s: String): Set[Char] = {
    val (start, end) = s.splitAt(s.length / 2)
    (for {
      a <- start
      b <- end
    } yield if (a == b) List(a) else Nil).flatten.toSet
  }

  private def value(c: Char): Int =
    if (c.isUpper)
      c.toInt - 'A'.toInt + 1 + 26
    else
      c.toInt - 'a'.toInt + 1
}
