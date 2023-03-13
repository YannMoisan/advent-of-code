import com.yannmoisan.util.grid.{Direction8, Grid1D, Pos}

object Day11 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = Grid1D(input.toArray)
    run(grid, pos => grid.dim.neighbors8(pos), 4)
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = Grid1D(input.toArray)
    run(grid, pos => Direction8.all.flatMap(dir => nextVisible(grid, pos, dir)).toArray, 5)
  }

  private def run(grid: Grid1D[Char], neighbors: Pos => Array[Pos], limit: Int): Int =
    Iterator
      .iterate(grid)(applyRules(neighbors, limit))
      .sliding(2).find(seq => seq(0) == seq(1)) match {
      case Some(seq) => seq(0).findAll('#').size
      case None      => sys.error("illegal state")
    }

  private def applyRules(neighbors: Pos => Array[Pos], limit: Int)(grid: Grid1D[Char]): Grid1D[Char] = {
    val newGrid = Grid1D.fill(grid.dim.width, grid.dim.height)('?')
    newGrid.dim.allPos.foreach { pos =>
      newGrid(pos) = grid(pos) match {
        case '.'                                                       => '.'
        case 'L' if neighbors(pos).count(p => grid(p) == '#') == 0     => '#'
        case '#' if neighbors(pos).count(p => grid(p) == '#') >= limit => 'L'
        case _                                                         => grid(pos)
      }
    }
    newGrid
  }

  private def nextVisible(grid: Grid1D[Char], start: Pos, dir: (Int, Int)): Option[Pos] =
    Iterator
      .unfold(start)(p => grid.dim.move(p, dir).map(x => (x, x)))
      .find(p => grid(p) != '.')
}
