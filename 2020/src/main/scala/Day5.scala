object Day5 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int =
    input.map(Binary.id).max

  override def part2(input: Iterator[String]): Int = {
    val ids = input.map(Binary.id).toSet
    val max = ids.max
    (0 to max).find(i => ids.contains(i - 1) && ids.contains(i + 1) && !ids.contains(i)) match {
      case Some(i) => i
      case None    => sys.error("illegal state")
    }

  }
}

object Binary extends App {
  def id(s: String): Int =
    s.foldLeft(0)((acc, c) => acc << 1 | (if (c == 'B' || c == 'R') 1 else 0))
}
