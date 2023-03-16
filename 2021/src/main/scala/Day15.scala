@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day15 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]) =
    dijkstra(input.toArray.map(_.toCharArray.map(_.asDigit)))

  override def part2(input: Iterator[String]) = dijkstra(fiveTimesLarger(input.toArray.map(_.toCharArray.map(_.asDigit))))

  def fiveTimesLarger(input: Array[Array[Int]]): Array[Array[Int]] =
    Array
      .tabulate(input.head.length * 5, input.length * 5) { (x, y) =>
        val dx = x / input.head.length
        val dy = y / input.length
        val value = input(y % input.head.length)(x % input.length) + dx + dy
        if (value > 9) value - 9 else value
      }

  def apply(lines: Array[Array[Int]], x: Int, y: Int): Int =
    lines(y)(x) //.asDigit

  def neighbors(mx: Int, my: Int, x: Int, y: Int): Seq[(Int, Int)] =
    Seq(
      (x + 1, y),
      (x - 1, y),
      (x, y + 1),
      (x, y - 1)
    ).filter { case (x, y) => x >= 0 && y >= 0 && x < mx && y < my }

  def dijkstra(grid: Array[Array[Int]]) = {
    val mx: Int = grid.head.length
    val my: Int = grid.length
    val dist    = Array.fill[Int](mx, my)(Int.MaxValue)
    dist(0)(0) = 0
    var toVisit: Set[(Int, Int)] = (for {
      x <- 0 until mx
      y <- 0 until my
    } yield (x, y)).toSet
    while (toVisit.size > 0) {
      // find min
      val s1 = toVisit.minBy { case (x, y) => dist(y)(x) }
      toVisit -= s1
      neighbors(mx, my, s1._1, s1._2)
        .foreach {
          case (x, y) =>
            if (dist(y)(x) > dist(s1._2)(s1._1) + apply(grid, x, y))
              dist(y)(x) = dist(s1._2)(s1._1) + apply(grid, x, y)
        }
    }
    dist(my - 1)(mx - 1)
  }
}
