import com.yannmoisan.util.grid.{Grid, Grid1D}

import scala.collection.mutable

object Day15 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]) =
    dijkstra(Grid1D(input.toArray.map(_.toCharArray.map(_.asDigit))))

  override def part2(input: Iterator[String]) =
    dijkstra(Grid1D(fiveTimesLarger(input.toArray.map(_.toCharArray.map(_.asDigit)))))

  def fiveTimesLarger(input: Array[Array[Int]]): Array[Array[Int]] =
    Array
      .tabulate(input.head.length * 5, input.length * 5) { (x, y) =>
        val dx    = x / input.head.length
        val dy    = y / input.length
        val value = input(y % input.head.length)(x % input.length) + dx + dy
        if (value > 9) value - 9 else value
      }

  def dijkstra(grid: Grid[Int]) = {
    val dist = Array.fill[Int](grid.dim.size)(Int.MaxValue)
    dist(0) = 0
    val toVisit = mutable.TreeSet[(Int, Int)](dist.zipWithIndex.toIndexedSeq: _*)

    while (toVisit.size > 0) {
      // find min
      val (d, s1) = toVisit.head
      val _       = toVisit.remove((d, s1))
      grid.dim.neighbors4(s1).foreach { j =>
        if (dist(j) > dist(s1) + grid(j)) {
          val _ = toVisit.remove((dist(j), j))
          dist(j) = dist(s1) + grid(j)
          val _ = toVisit.addOne((dist(j), j))
        }
      }
    }
    dist(grid.dim.size - 1)
  }
}
