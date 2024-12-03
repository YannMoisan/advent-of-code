object Day3 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val mulRegex = """mul\((\d{1,3}),(\d{1,3})\)""".r
    mulRegex
      .findAllMatchIn(input.mkString)
      .foldLeft(0) { case (sum, m) => sum + m.group(1).toInt * m.group(2).toInt }
  }

  override def part2(input: Iterator[String]): Int = {
    val mulRegex  = """mul\((\d{1,3}),(\d{1,3})\)""".r
    val dontRegex = """don't\(\)(.*?)(do\(\)|$)"""
    mulRegex
      .findAllMatchIn(input.mkString.replaceAll(dontRegex, ""))
      .foldLeft(0) { case (sum, m) => sum + m.group(1).toInt * m.group(2).toInt }
  }
}
