object Day7 extends SinglePuzzle[Int, Int] {
  override def part1(input: String): Int = {
    val arr = input.split(",").map(_.toInt)
    (0 until 1000).map(i => arr.map(e => math.abs(i - e)).sum).min
  }

  override def part2(input: String): Int = {
    val arr = input.split(",").map(_.toInt)
    (0 until 1000)
      .map(i =>
        arr.map { e =>
          val n = math.abs(i - e)
          (n * (n + 1)) / 2
        }.sum
      ).min
  }
}
