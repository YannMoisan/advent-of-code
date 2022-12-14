@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day8 extends MultiPuzzle[Int, Int]{
  override def part1(input: Iterator[String]): Int = {
    val grid = new Grid(input.toArray.map(_.toArray))
    (for {
      x <- grid.xindices
      y <- grid.yindices
      if isVisible(grid, Pos(x,y))
    } yield Pos(x,y)).size
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = new Grid(input.toArray.map(_.toArray))
    (for {
      x <- 1 until grid.width - 1
      y <- 1 until grid.height - 1
    } yield score(grid, Pos(x,y))).max
  }

  private def isVisible(grid: Grid, p: Pos) =
    views(grid, p).exists(xs => xs.forall(x => grid(x) < grid(p)))

  private def views(grid: Grid, p: Pos) = Seq(
    (0 until p.y).map(Pos(p.x, _)).reverse,
    (p.y + 1 until grid.height).map(Pos(p.x, _)),
    (0 until p.x).map(Pos(_, p.y)).reverse,
    (p.x + 1 until grid.width).map(Pos(_, p.y))
  )

  private def score(grid: Grid, p: Pos) = {
    views(grid, p)
      .map { xs => math.min(xs.length, xs.takeWhile { pos => grid(pos) < grid(p) }.length + 1) }
      .product
  }
}
