object Day24 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val dominos = input.map { case s"$a/$b" => (a.toInt, b.toInt) }.toList
    search1(dominos.toSet, 0)
  }

  def search1(available: Set[(Int, Int)], free: Int): Int =
    available
      .filter { case (a, b) => a == free || b == free }
      .map(t => t._1 + t._2 + search1(available - t, if (t._1 == free) t._2 else t._1))
      .maxOption
      .getOrElse(0)

  override def part2(input: Iterator[String]): Int = {
    val dominos = input.map { case s"$a/$b" => (a.toInt, b.toInt) }.toList
    search2(dominos.toSet, 0)._2
  }

  def search2(available: Set[(Int, Int)], free: Int): (Int, Int) =
    available
      .filter { case (a, b) => a == free || b == free }
      .map { t =>
        val ret = search2(available - t, if (t._1 == free) t._2 else t._1)
        (1 + ret._1, t._1 + t._2 + ret._2)
      }
      .maxOption
      .getOrElse((0, 0))
}
