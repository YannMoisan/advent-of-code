import com.yannmoisan.util.grid.{Direction8, Grid2D, Pos}

object Day11 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = new Grid2D(input.toArray.map(_.toArray))
    run(grid, pos => grid.dim.neighbors8(pos), 4)
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = new Grid2D(input.toArray.map(_.toArray))
    run(grid, pos => Direction8.all.flatMap(dir => nextVisible(grid, pos, dir)).toArray, 5)
  }

  private def run(grid: Grid2D[Char], neighbors: Pos => Array[Pos], limit: Int): Int =
    Iterator
      .iterate(grid)(applyRules(neighbors, limit))
      .sliding(2).find(seq => seq(0) == seq(1)) match {
      case Some(seq) => seq(0).findAll('#').size
      case None      => sys.error("illegal state")
    }

  private def applyRules(neighbors: Pos => Array[Pos], limit: Int)(grid: Grid2D[Char]): Grid2D[Char] = {
    val newGrid = Grid2D.fill(grid.dim.width, grid.dim.height)('?')
    newGrid.dim.allPos.foreach { pos =>
      grid(pos) match {
        case '.'                                                       => newGrid(pos) = '.'
        case 'L' if neighbors(pos).count(p => grid(p) == '#') == 0     => newGrid(pos) = '#'
        case '#' if neighbors(pos).count(p => grid(p) == '#') >= limit => newGrid(pos) = 'L'
        case _                                                         => newGrid(pos) = grid(pos)
      }
    }
    newGrid
  }

  private def nextVisible(grid: Grid2D[Char], start: Pos, dir: (Int, Int)): Option[Pos] =
    Iterator
      .unfold(start)(p => grid.dim.move(p, dir).map(x => (x, x)))
      .find(p => grid(p) != '.')
}
