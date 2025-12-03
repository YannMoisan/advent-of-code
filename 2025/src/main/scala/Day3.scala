object Day3 extends MultiPuzzle[Int, Long] {

  override def part1(input: Iterator[String]): Int =
    input.map { line =>
      val (fst, next) = findMaxCharAndNextString(line, 2)
      val (snd, _)    = findMaxCharAndNextString(next, 1)
      Seq(fst, snd).mkString.toInt
    }.sum

  override def part2(input: Iterator[String]): Long =
    input.map { line =>
      val results = (12 to 1 by -1).scanLeft(('0', line)) {
        case ((_, s), i) => findMaxCharAndNextString(s, i)
      }
      results.tail.map(_._1).mkString.toLong
    }.sum

  private def findMaxCharAndNextString(s: String, remaining: Int): (Char, String) = {
    val max = s.substring(0, s.length - remaining + 1).max
    val pos = s.indexOf(max.toInt)
    (max, s.substring(pos + 1))
  }
}
