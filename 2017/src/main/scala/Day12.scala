import com.yannmoisan.util.graph.BFS

object Day12 extends MultiPuzzle[Int, Int] {

  override def part1(lines: Iterator[String]): Int = {
    val graph  = parse(lines)
    val stream = BFS.breadth_first_traverse("0", graph)
    stream.map(_._1).distinct.size
  }

  override def part2(lines: Iterator[String]): Int = {
    val graph = parse(lines)
    (0 until 2000)
      .map(i => BFS.breadth_first_traverse(i.toString, graph).map(_._1).toSet)
      .distinct.size
  }

  private def parse(lines: Iterator[String]): Map[String, Seq[String]] =
    lines
      .map(_.split(" <-> "))
      .map { case Array(a, b) => (a, b.split(", ").toSeq) }
      .toMap

}
