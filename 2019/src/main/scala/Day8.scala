object Day8 extends SinglePuzzle[Int, String] {
  override def part1: String => Int = { line =>
    val groups   = line.grouped(25 * 6).toArray
    val selected = groups.minBy(_.count(_ == '0'))
    selected.count(_ == '1') * selected.count(_ == '2')
  }

  override def part2: String => String = { line =>
    val groups: Array[String] = line.grouped(25 * 6).toArray
    val s: String = (0 until 25 * 6)
      .map { i =>
        (0 until groups.length)
          .map { j =>
            groups(j)(i)
          }
          .find(_ != '2')
          .get
      }
      .mkString("")
    s.replace('0', ' ')
      .replace('1', '*')
      .grouped(25)
      .mkString("\n")
  }

}
