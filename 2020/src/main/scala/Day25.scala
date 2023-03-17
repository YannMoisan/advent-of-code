object Day25 extends MultiPuzzle[Long, Int] {
  override def part1(input: Iterator[String]): Long = {
    val v1 = 6270530
    val v2 = 14540258

    val size2 = Iterator.iterate(1L)(next(7)).indexOf(v2)
    Iterator.iterate(1L)(next(v1)).drop(size2).next()
  }

  def next(subjectNumber: Int)(v: Long): Long =
    (v * subjectNumber) % 20201227

  override def part2(input: Iterator[String]): Int = 42
}
