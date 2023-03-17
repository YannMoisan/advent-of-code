import com.yannmoisan.util.grid.{Grid, Grid1D, Pos}

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day8 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = Grid1D(input.toArray)
    grid.dim.indices.filter(p => isVisible(grid, p)).size
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = Grid1D(input.toArray)
    (for {
      x <- 1 until grid.dim.width - 1
      y <- 1 until grid.dim.height - 1
    } yield score(grid, Pos(x, y)(grid.dim).index)).max
  }

  private def isVisible(grid: Grid[Char], p: Int) =
    views(grid, p).exists(xs => xs.forall(x => grid(x.index) < grid(p)))

  private def views(grid: Grid[Char], index: Int) = {
    val p = grid.dim.pos(index)
    Seq(
      (0 until p.y).map(Pos(p.x, _)(grid.dim)).reverse,
      (p.y + 1 until grid.dim.height).map(Pos(p.x, _)(grid.dim)),
      (0 until p.x).map(Pos(_, p.y)(grid.dim)).reverse,
      (p.x + 1 until grid.dim.width).map(Pos(_, p.y)(grid.dim))
    )
  }

  private def score(grid: Grid[Char], p: Int) =
    views(grid, p).map { xs =>
      math.min(xs.length, xs.takeWhile(pos => grid(pos.index) < grid(p)).length + 1)
    }.product
}
