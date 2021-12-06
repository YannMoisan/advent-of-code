object Day6 extends SinglePuzzle[Int, Long] {
  override def part1(input: String): Int = {
    val ages: List[Int] = input.split(",").map(_.toInt).toList
    val end             = (0 until 80).foldLeft(ages) { case (acc, _) => play(acc) }
    end.size
  }

  def play(ages: List[Int]): List[Int] =
    ages.flatMap { i =>
      if (i != 0)
        Seq(i - 1)
      else
        Seq(6, 8)
    }

  override def part2(input: String): Long = {
    val ages: List[Long] = input.split(",").map(_.toLong).toList
    val arr: Array[Long] = Array.tabulate(9)(i => ages.count(_ == i).toLong)
    (0 until 256).foreach { _ =>
      val tmp = arr(0)
      arr(0) = arr(1)
      arr(1) = arr(2)
      arr(2) = arr(3)
      arr(3) = arr(4)
      arr(4) = arr(5)
      arr(5) = arr(6)
      arr(6) = arr(7) + tmp
      arr(7) = arr(8)
      arr(8) = tmp
    }

    arr.sum
  }
}
