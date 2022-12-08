@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day8 extends MultiPuzzle[Int, Int]{
  override def part1(input: Iterator[String]): Int = {
    val grid: Array[Array[Int]] = input.toArray.map(s => s.map(c => c.toString.toInt).toArray)
    (for {
      row <- grid.indices
      col <- grid.head.indices
      if isVisible(grid, row, col)
    } yield (row, col)).size
  }

  override def part2(input: Iterator[String]): Int = {
    val grid: Array[Array[Int]] = input.toArray.map(s => s.map(c => c.toString.toInt).toArray)
    (for {
      row <- 1 until grid.length - 1
      col <- 1 until grid.head.length - 1
    } yield score(grid, row, col)).max
  }

  private def isVisible(grid: Array[Array[Int]], r: Int, c: Int) =
    views(grid, r, c).exists(xs => xs.forall(x => grid(x._1)(x._2) < grid(r)(c)))

  private def views(grid: Array[Array[Int]], r: Int, c: Int) = Seq(
    (0 until r).map((_, c)).reverse,
    (r + 1 until grid.head.length).map((_, c)),
    (0 until c).map((r, _)).reverse,
    (c + 1 until grid.length).map((r, _))
  )

  private def score(grid: Array[Array[Int]], r: Int, c: Int) = {
    views(grid, r, c)
      .map { xs => math.min(xs.length, xs.takeWhile { pos => grid(pos._1)(pos._2) < grid(r)(c) }.length + 1) }
      .product
  }
}
