import scala.collection.mutable.ArrayBuffer

object Day25 extends MultiPuzzle[Int, Int] {
  case class Position4(x: Int, y: Int, z: Int, t: Int)

  object Position4 {
    def manhattan(a: Position4, b: Position4): Int =
      math.abs(b.x - a.x) + math.abs(b.y - a.y) + math.abs(b.z - a.z) + math.abs(b.t - a.t)
  }

  class Cluster(val positions: ArrayBuffer[Position4]) {
    def isIn(p: Position4) = positions.exists(Position4.manhattan(_, p) <= 3)
  }

  override def part1(input: Iterator[String]): Int = {
    val positions: Array[Position4] = input.map {
      _ match {
        case s"$x,$y,$z,$t" => Position4(x.toInt, y.toInt, z.toInt, t.toInt)
      }
    }.toArray
    clusterize(positions).length
  }

  private def clusterize(positions: Array[Position4]): ArrayBuffer[Cluster] = {
    var clusters = ArrayBuffer[Cluster]()
    positions.foreach { p =>
      val inRangeClusters = clusters.filter(_.isIn(p))
      inRangeClusters.size match {
        case 0 => clusters.addOne(new Cluster(ArrayBuffer(p)))
        case 1 => inRangeClusters.head.positions.addOne(p)
        case _ =>
          clusters = clusters.diff(inRangeClusters)
          clusters.addOne(new Cluster(p +: inRangeClusters.flatMap(_.positions)))
      }
    }
    clusters
  }

  override def part2(input: Iterator[String]): Int = 42
}
