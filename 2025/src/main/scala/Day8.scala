import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day8 extends MultiPuzzle[Long, Long] {

  case class Clusters(clusters: mutable.ListBuffer[Cluster]) {
    def addLink(id1: Int, id2: Int) = {
      // find cluster1
      val c1 = clusters.find(c => c.points.contains(id1)).get
      // find cluster2
      val c2 = clusters.find(c => c.points.contains(id2)).get
      // merge clusters
      if (c1 != c2) {
        c1.points.addAll(c2.points)
        c2.points.--=(c2.points)
      }
    }
    def print(): Unit =
      clusters.foreach(c => println(c.points.mkString(",")))
  }
  case class Cluster(points: mutable.Set[Int])

  case class Point(x: Long, y: Long, z: Long)

  object Point {
    def dist2(a: Point, b: Point): Long =
      (b.x - a.x) * (b.x - a.x) + (b.y - a.y) * (b.y - a.y) + (b.z - a.z) * (b.z - a.z)
  }

  override def part1(input: Iterator[String]): Long = {
    val points = input.map { s =>
      val Array(x, y, z) = s.split(',').map(_.toLong)
      Point(x, y, z)
    }.toArray
    val distances = for {
      i <- 0 until points.size
      j <- (i + 1) until points.size
    } yield (Point.dist2(points(i), points(j)), i, j)
    val shortest = distances.sortBy(_._1).take(1000)

    val clusters0 = points.indices.map(i => Cluster(mutable.Set[Int](i)))
    val clusters  = Clusters(ListBuffer[Cluster](clusters0: _*))

    shortest.foreach { case (_, i, j) => clusters.addLink(i, j) }

    clusters.clusters.map(_.points.size).sortBy(x => -x).take(3).product.toLong

  }

  override def part2(input: Iterator[String]): Long = {
    val points = input.map { s =>
      val Array(x, y, z) = s.split(',').map(_.toLong)
      Point(x, y, z)
    }.toArray
    val distances = for {
      i <- 0 until points.size
      j <- (i + 1) until points.size
    } yield (Point.dist2(points(i), points(j)), i, j)
    val shortest = distances.sortBy(_._1)

    val clusters0 = points.indices.map(i => Cluster(mutable.Set[Int](i)))
    val clusters  = Clusters(ListBuffer[Cluster](clusters0: _*))

    var i   = 0
    var cur = shortest(0)
    while (!clusters.clusters.exists(_.points.size == 1000)) {
      cur = shortest(i)
      clusters.addLink(cur._2, cur._3)
      i += 1
    }
    points(cur._2).x * points(cur._3).x

  }
}
