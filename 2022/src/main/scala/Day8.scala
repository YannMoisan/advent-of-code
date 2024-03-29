import com.yannmoisan.util.grid.{Grid, Grid1D, Pos}

object Day8 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = Grid1D(input)
    grid.dim.indices.filter(p => isVisible(grid, grid.dim.pos(p))).size
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = Grid1D(input)
    (for {
      x <- 1 until grid.dim.width - 1
      y <- 1 until grid.dim.height - 1
    } yield score(grid, Pos(x, y))).max
  }

  private def isVisible(grid: Grid[Char], p: Pos) =
    views(grid, p).exists(xs => xs.forall(x => grid(x) < grid(p)))

  private def views(grid: Grid[Char], p: Pos) =
    Seq(
      (0 until p.y).map(Pos(p.x, _)).reverse,
      (p.y + 1 until grid.dim.height).map(Pos(p.x, _)),
      (0 until p.x).map(Pos(_, p.y)).reverse,
      (p.x + 1 until grid.dim.width).map(Pos(_, p.y))
    )

  private def score(grid: Grid[Char], p: Pos) =
    views(grid, p).map { xs =>
      math.min(xs.length, xs.takeWhile(pos => grid(pos) < grid(p)).length + 1)
    }.product
}
