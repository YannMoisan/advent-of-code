object Day22 extends MultiPuzzle[Int, String] {
  val node = """/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%""".r

  case class Node(pos: (Int, Int), used: Int, avail: Int) {
    def toChar = if (used > 400) '#' else if (used == 0) '_' else '.'
  }

  def parse(s: String): Node = s match {
    case node(x, y, _, u, a, _) => Node((x.toInt, y.toInt), u.toInt, a.toInt)
  }

  override def part1(lines: Iterator[String]): Int = {
    val nodes = lines.toList.tail.tail.map(parse)
    (for {
      n1 <- nodes
      n2 <- nodes
      if n1.pos != n2.pos
      if n1.used <= n2.avail
      if n1.used != 0
    } yield 1).sum
  }

  override def part2(lines: Iterator[String]): String = {
    val nodes = lines.toList.tail.tail.map(parse).map(node => (node.pos, node)).toMap
    (0 to 30).map(y => (0 to 33).map(x => nodes((x, y)).toChar).mkString).mkString("\n")
  }
}
