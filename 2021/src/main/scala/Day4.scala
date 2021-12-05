// grids are represented as 2d mutable array
object Day4 extends MultiPuzzle[Int, Int] with App {
  case class State(turn: Int)

  override def part1(input: Iterator[String]): Int = {
    val lines                                 = input.toArray
    val numbers: Array[Int]                   = lines.head.split(",").map(_.toInt)
    val groupedLines: Iterator[Array[String]] = lines.drop(2).grouped(6)
    val grids: Array[Array[Array[Int]]]       = groupedLines.map(parse).toArray

    val end = loop(State(0))(s => {
      grids.foreach(grid => draw(grid, numbers(s.turn)))
      State(s.turn + 1)
    }, _ => grids.exists(grid => hasCompletedRow(grid) || hasCompletedCol(grid)))

    val winner = grids.find(grid => hasCompletedRow(grid) || hasCompletedCol(grid))
    winner.fold(0)(sumOfAllNonMarkedNumbers) * numbers(end.turn - 1)
  }

  override def part2(input: Iterator[String]): Int = {
    var all                                   = (0 until 100).toSet
    val lines                                 = input.toArray
    val numbers: Array[Int]                   = lines.head.split(",").map(_.toInt)
    val groupedLines: Iterator[Array[String]] = lines.drop(2).grouped(6)
    val grids: Array[Array[Array[Int]]]       = groupedLines.map(parse).toArray

    val end = loop(State(0))(
      s => {
        grids.foreach(grid => draw(grid, numbers(s.turn)))
        if (all.size != 1) {
          (0 until grids.length).foreach { i =>
            if (hasCompletedRow(grids(i)) || hasCompletedCol(grids(i))) {
              all = all - i
            }
          }
        }
        State(s.turn + 1)
      },
      _ => grids.count(grid => hasCompletedRow(grid) || hasCompletedCol(grid)) == grids.length
    )

    val winner = all.headOption
    winner.fold(0)(i => sumOfAllNonMarkedNumbers(grids(i))) * numbers(end.turn - 1)
  }

  private def parse(arr: Array[String]): Array[Array[Int]] =
    (0 until 5).map(i => arr(i).trim.split("\\s+").map(_.toInt)).toArray

  private def hasCompletedRow(grid: Array[Array[Int]]): Boolean =
    (0 until 5).exists(i => grid(i).forall(_ == 0))

  private def hasCompletedCol(grid: Array[Array[Int]]): Boolean =
    (0 until 5).exists(i => (0 until 5).forall(j => grid(j)(i) == 0))

  private def sumOfAllNonMarkedNumbers(grid: Array[Array[Int]]): Int =
    grid.map(_.sum).sum

  private def draw(grid: Array[Array[Int]], value: Int): Unit =
    for {
      i <- 0 until 5
      j <- 0 until 5
    } { if (grid(i)(j) == value) grid(i)(j) = 0 }

  private def loop[S](init: S)(f: S => S, isEnd: S => Boolean): S = {
    val next = f(init)
    if (isEnd(next)) next else loop(next)(f, isEnd)
  }
}
