object Day4 extends MultiPuzzle[Int, Int] {
  def valid(f: String => String): Iterator[String] => Int = { lines: Iterator[String] =>
    lines.map(line => if (line.split(" ").groupBy(f).exists(_._2.size > 1)) 0 else 1).sum
  }

  override def part1(lines: Iterator[String]): Int = valid(identity)(lines)

  override def part2(lines: Iterator[String]): Int = valid(_.sorted)(lines)
}
