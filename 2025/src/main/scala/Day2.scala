object Day2 extends SinglePuzzle[Long, Long] {
  override def part1(input: String): Long =
    common(input, isInvalid1)
  override def part2(input: String): Long =
    common(input, isInvalid2)

  private def common(input: String, isInvalid: Long => Boolean): Long = {
    val ranges = input.split(',').map { s =>
      val Array(a, b) = s.split('-')
      a.toLong to b.toLong
    }
    ranges.map(_.filter(isInvalid).sum).sum
  }

  private def isInvalid1(l: Long) = {
    val s = l.toString
    s.length % 2 == 0 && existsRepeatedGroup(s, s.length / 2)
  }

  private def isInvalid2(l: Long) = {
    val s = l.toString
    (1 until s.length).exists(existsRepeatedGroup(s, _))
  }

  private def existsRepeatedGroup(s: String, size: Int): Boolean =
    s.length % size == 0 && s.grouped(size).distinct.length == 1

}
