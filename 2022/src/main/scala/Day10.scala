object Day10 extends MultiPuzzle[Int, String] {
  override def part1(input: Iterator[String]): Int = {
    val cycles = (20 to 220 by 40).toSet
    values(input)
      .zipWithIndex
      .map { case (value, cycle) => (value, cycle + 1) }
      .collect { case (value, cycle) if cycles.contains(cycle) => cycle * value }
      .sum
  }

  override def part2(input: Iterator[String]): String = {
    values(input).zipWithIndex
      .map { case (value, cycle) => if (math.abs(cycle % 40 - value) <= 1) '#' else '.' }
      .grouped(40)
      .map(_.mkString)
      .take(6)
      .mkString("\n")
  }

  private def values(input: Iterator[String]): Seq[Int] = {
    var value = 1
    input.flatMap {
      case s"addx $v" =>
        val s = Seq(value, value)
        value += v.toInt
        s
      case "noop" =>
        Seq(value)
    }.toSeq
  }
}
