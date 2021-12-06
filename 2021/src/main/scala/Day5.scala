object Day5 extends MultiPuzzle[Int, Int] with App {
  override def part1(input: Iterator[String]): Int =
    solve(input, horOrVer)

  override def part2(input: Iterator[String]): Int =
    solve(input, _ => true)

  private def solve(input: Iterator[String], pred: ((Int, Int, Int, Int)) => Boolean): Int =
    input
      .map(parse)
      .filter(pred)
      .flatMap(allPoints)
      .toList
      .groupBy(identity)
      .view
      .mapValues(_.length)
      .count(_._2 > 1)

  private def parse(line: String): (Int, Int, Int, Int) = {
    val s"$x1,$y1 -> $x2,$y2" = line
    (x1.toInt, y1.toInt, x2.toInt, y2.toInt)
  }

  def horOrVer(line: (Int, Int, Int, Int)): Boolean = {
    val (x1, y1, x2, y2) = line
    x1 == x2 || y1 == y2
  }

  private def allPoints(line: (Int, Int, Int, Int)): Seq[(Int, Int)] = {
    val (x1, y1, x2, y2) = line
    val dx               = x2 - x1
    val dy               = y2 - y1
    val dist             = math.max(math.abs(dx), math.abs(dy))
    (0 to dist).map(i => (x1 + i * dx / dist, y1 + i * dy / dist))
  }
}
