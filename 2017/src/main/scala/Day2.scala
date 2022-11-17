object Day2 extends MultiPuzzle[Int, Int] {
  override def part1(lines: Iterator[String]) : Int = {
    lines.map { line =>
      val numbers = line.split("\t").map(_.toInt)
      numbers.max - numbers.min
    }.sum
  }

  override def part2(lines: Iterator[String]) : Int = {
    lines.map { line =>
      val numbers = line.split("\t").map(Integer.parseInt)
      val pairs = numbers.combinations(2)
      pairs.map { arr =>
        val Array(a, b) = arr.sorted
        if (b % a == 0)
          b / a
        else
          0
      }.sum
    }.sum

  }
}
