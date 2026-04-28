import com.yannmoisan.util.grid.{Grid1D, Pos}

object Day11 extends MultiPuzzle[Long, Long] {

  override def part1(input: Iterator[String]): Long = common(input, 2L)
  override def part2(input: Iterator[String]): Long = common(input, 1000000L)

  private def common(input: Iterator[String], size: Long): Long = {
    val grid = Grid1D(input)
    val all  = grid.findAll('#')
    val emptyCols = (0 until grid.dim.width).filter { col =>
      (0 until grid.dim.height).forall(row => grid(grid.dim.index(Pos(col, row))) == '.')
    }
    val emptyRows = (0 until grid.dim.height).filter { row =>
      (0 until grid.dim.width).forall(col => grid(grid.dim.index(Pos(col, row))) == '.')
    }
    all
      .combinations(2).map { case Vector(from, to) =>
        val fromPos = grid.dim.pos(from)
        val toPos   = grid.dim.pos(to)
        val xDist = (math.min(fromPos.x, toPos.x) until math.max(fromPos.x, toPos.x)).map { x =>
          if (emptyCols.contains(x)) size else 1L
        }.sum
        val yDist = (math.min(fromPos.y, toPos.y) until math.max(fromPos.y, toPos.y)).map { y =>
          if (emptyRows.contains(y)) size else 1L
        }.sum
        xDist + yDist
      }.sum
  }
}
