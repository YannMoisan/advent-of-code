object Day6 extends SinglePuzzle[Int, Int] {

  def nextBank(s: IndexedSeq[Int]): IndexedSeq[Int] = {
    val max   = s.max
    val index = s.indexOf(max)

    (1 to max).foldLeft(s.updated(index, 0)) { case (s2, i) =>
      val indexToUpdate = (index + i) % s.size
      s2.updated(indexToUpdate, s2(indexToUpdate) + 1)
    }
  }

  def realloc: String => Seq[(IndexedSeq[Int], Int)] = { line =>
    val s0 = line.split("\t").map(Integer.parseInt).toIndexedSeq

    val allBanks = (1 to 10000).scanLeft(s0) { case (b, _) => nextBank(b) }

    val z = allBanks.zipWithIndex.groupBy(_._1)
    z.filter { case (_, v) => v.size > 1 }.values.minBy(a => a(1)._2)
  }

  override def part1(line: String): Int =
    realloc(line)(1)._2

  override def part2(line: String): Int = {
    val t = realloc(line)
    t(1)._2 - t(0)._2
  }

}
