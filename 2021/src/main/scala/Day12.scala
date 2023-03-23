import com.yannmoisan.util.graph.BFS

object Day12 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int =
    bfsPart1(parseEdges(input.toArray))

  override def part2(input: Iterator[String]): Int =
    bfsPart2(parseEdges(input.toArray))

  def parseEdges(in: Array[String]): Map[String, Array[String]] =
    in.flatMap { line =>
        val Array(from, to) = line.split("-")
        Seq((from, to), (to, from))
      }.groupMap(_._1)(_._2)

  case class State(cur: String, history: List[String])

  def derive1(graph: String => Array[String]): State => Seq[State] = { s: State =>
    graph(s.cur)
      .filterNot(str => str.toLowerCase == str && s.history.contains(str))
      .toIndexedSeq.map(next => State(next, next :: s.history))
  }

  def derive2(graph: String => Array[String]): State => Seq[State] = { s: State =>
    graph(s.cur)
      .filterNot(str =>
        (str.toLowerCase == str) &&
          (s.history.contains(str) && (
            Seq("start", "end").contains(str) ||
              s.history
                .filter(node => node.toLowerCase == node).groupBy(identity).exists(_._2.size > 1)
          ))
      )
      .toIndexedSeq.map(next => State(next, next :: s.history))
  }

  def bfsPart1(edges: Map[String, Array[String]]): Int =
    BFS
      .breadth_first_traverse(State("start", List("start")), derive1(edges))
      .filter(_._1.cur == "end")
      .size

  def bfsPart2(edges: Map[String, Array[String]]): Int =
    BFS
      .breadth_first_traverse(State("start", List("start")), derive2(edges))
      .filter(_._1.cur == "end")
      .size
}
