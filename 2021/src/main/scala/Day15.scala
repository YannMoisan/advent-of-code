import com.yannmoisan.util.grid.{Grid, Grid1D, Pos}

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day15 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]) =
    dijkstra(Grid1D(input.toArray.map(_.toCharArray.map(_.asDigit))))

  override def part2(input: Iterator[String]) = dijkstra(Grid1D(fiveTimesLarger(input.toArray.map(_.toCharArray.map(_.asDigit)))))

  def fiveTimesLarger(input: Array[Array[Int]]): Array[Array[Int]] =
    Array
      .tabulate(input.head.length * 5, input.length * 5) { (x, y) =>
        val dx = x / input.head.length
        val dy = y / input.length
        val value = input(y % input.head.length)(x % input.length) + dx + dy
        if (value > 9) value - 9 else value
      }

  def dijkstra(grid: Grid[Int]) = {
    val dist    = Array.fill[Int](grid.dim.width * grid.dim.height)(Int.MaxValue)
    dist(0) = 0
    var toVisit = grid.dim.indices.toSet
    while (toVisit.size > 0) {
      // find min
      val s1 = toVisit.minBy { index => dist(index) }
      toVisit -= s1
      grid.dim.neighbors4(s1).foreach { j =>
            if (dist(j) > dist(s1) + grid(j))
              dist(j) = dist(s1) + grid(j)
        }
    }
    dist(Pos(grid.dim.width - 1, grid.dim.height - 1)(grid.dim).index)
  }
}
