object Day25 extends MultiPuzzle[Long, Int] {
  override def part1(input: Iterator[String]): Long = {
    val v1 = 6270530
    val v2 = 14540258

    //val Some((_, size1)) = loopSize(v1, 7)
    val Some((_, size2)) = loopSize(v2, 7)

    Iterator.iterate(1L)(next(v1)).drop(size2).next()
  }

  def next(subjectNumber: Int)(v: Long): Long =
    (v * subjectNumber) % 20201227

  def loopSize(v: Int, subjectNumber: Int): Option[(Long, Int)] =
    Iterator.iterate(1L)(next(subjectNumber)).zipWithIndex.find(_._1 == v)

  override def part2(input: Iterator[String]): Int = ???
}
