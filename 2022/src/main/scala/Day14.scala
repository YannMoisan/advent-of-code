import com.yannmoisan.util.grid.{Direction4, Grid, Grid1D, Pos}

object Day14 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = makeGrid(input)
    fill(grid)
    grid.findAll('o').size
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = makeGrid(input)
    // add floor
    (0 until grid.dim.width).foreach(x => grid(Pos(x, grid.dim.height - 1)) = '#')
    fill(grid)
    grid.findAll('o').size
  }

  private def fill(grid: Grid[Char]) = {
    def nextPos(p: Pos): Option[Pos] =
      Seq(
        grid.dim.moveS(grid.dim.index(p), Direction4.Down).get, // one step down ?
        grid.dim.moveS(grid.dim.index(p), Direction4.Left).get, // one step down ?
        grid.dim.moveS(grid.dim.index(p), Direction4.Right).get // one step down ?
      ).find(p => grid(p) == '.').map(grid.dim.pos)

    var sand    = Pos(500, 0)
    var blocked = false
    while (!blocked && sand.y < grid.dim.height - 1)
      nextPos(sand) match {
        case Some(p) => sand = p
        case None =>
          if (sand == Pos(500, 0))
            blocked = true
          grid(sand) = 'o'
          sand = Pos(500, 0)
      }
  }

  private def makeGrid(input: Iterator[String]): Grid1D[Char] = {
    def parse(s: String): Array[(Int, Int)] =
      s.split(" -> ").map { seg =>
        val Array(x, y) = seg.split(",").map(_.toInt)
        (x, y)
      }

    def allPositions(start: Pos, end: Pos) =
      if (start.x == end.x) {
        (start.y to end.y by math.signum(end.y - start.y)).map(y => Pos(start.x, y))
      } else {
        (start.x to end.x by math.signum(end.x - start.x)).map(x => Pos(x, start.y))
      }

    val paths: Seq[Array[(Int, Int)]] = input.map(parse).toList
    val maxY                          = paths.flatten.maxBy(t => t._2)._2
    val grid                          = Grid1D.fill(1000, maxY + 3)('.')

    // add paths on the grid
    paths.foreach { path =>
      path.sliding(2).foreach { case Array(start, end) =>
        allPositions(Pos(start._1, start._2), Pos(end._1, end._2))
          .foreach(pos => grid(pos) = '#')
      }
    }
    grid
  }
}
