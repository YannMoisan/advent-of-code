@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day15 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]) =
    dijkstra(input.toArray)

  override def part2(input: Iterator[String]) = dijkstra(fiveTimesLarger(input.toArray))

  def fiveTimesLarger(input: Array[String]): Array[String] =
    Array
      .tabulate(input.head.length * 5, input.length * 5) { (x, y) =>
        val dx = x / input.head.length
        val dy = y / input.length
        applyNTimes(input(y % input.head.length)(x % input.length), dx + dy)(next)
      }.map(_.mkString)

  // TODO create a combinator
  def applyNTimes[A](s: Char, n: Int)(f: Char => Char): Char = {
    var i   = 0
    var cur = s
    while (i < n) {
      cur = f(cur)
      i += 1
    }
    cur
  }

  def next(s: Char): Char =
    s match {
      case '1' => '2'
      case '2' => '3'
      case '3' => '4'
      case '4' => '5'
      case '5' => '6'
      case '6' => '7'
      case '7' => '8'
      case '8' => '9'
      case '9' => '1'
    }

  def apply(lines: Array[String], x: Int, y: Int): Int =
    Integer.parseInt(lines(y)(x).toString)

  def neighbors(mx: Int, my: Int, x: Int, y: Int): Seq[(Int, Int)] =
    Seq(
      (x + 1, y),
      (x - 1, y),
      (x, y + 1),
      (x, y - 1)
    ).filter { case (x, y) => x >= 0 && y >= 0 && x < mx && y < my }

  def dijkstra(grid: Array[String]) = {
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
