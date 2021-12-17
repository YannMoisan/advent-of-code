import scala.collection.mutable

object Day12 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int =
    bfsPart1(parseEdges(input.toArray))

  override def part2(input: Iterator[String]): Int =
    bfsPart2(parseEdges(input.toArray))

  def parseEdges(in: Array[String]): Seq[(String, String)] =
    in.flatMap { line =>
      val Array(from, to) = line.split("-")
      Seq((from, to), (to, from))
    }.toSeq

  def bfsPart1(edges: Seq[(String, String)]): Int = {
    // nodes to visit with the path to get there
    var pathsCount = 0

    val q = mutable.Queue[(String, List[String])]()
    val _ = q.enqueue(("start", List("start")))
    while (!q.isEmpty) {
      val (toVisit, path) = q.dequeue()
      // could be speed-up with adjacency list
      edges.collect { case (from, to) if from == toVisit => to }.foreach { v =>
        if ((v.toLowerCase == v) && path.contains(v)) {} else {
          if (v == "end") {
            pathsCount += 1
          }
          val _ = q.enqueue((v, v :: path))
        }
      }
    }
    pathsCount
  }

  def bfsPart2(edges: Seq[(String, String)]): Int = {
    // nodes to visit with the path to get there
    var pathsCount = 0

    val q = mutable.Queue[(String, List[String])]()
    val _ = q.enqueue(("start", List("start")))
    while (!q.isEmpty) {
      val (toVisit, path) = q.dequeue()
      // could be speed-up with adjacency list
      edges.collect { case (from, to) if from == toVisit => to }.foreach { v =>
        if ((v.toLowerCase == v) &&
            (path.contains(v) && (
              Seq("start", "end").contains(v) ||
              path.filter(node => node.toLowerCase == node).groupBy(identity).exists(_._2.size > 1)
            ))) // count small cave
          {} else {
          if (v == "end") {
            pathsCount += 1
          }
          val _ = q.enqueue((v, v :: path))
        }
      }
    }
    pathsCount
  }
}
