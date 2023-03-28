import com.yannmoisan.util.grid.{Direction8, DirectionWithIndex, Grid, Grid1D}

object Day11 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = Grid1D(input)
    run(grid, pos => grid.dim.neighbors8(pos), 4)
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = Grid1D(input)
    run(grid, pos => Direction8.all.flatMap(dir => nextVisible(grid, pos, dir)).toArray, 5)
  }

  private def run(grid: Grid[Char], neighbors: Int => Array[Int], limit: Int): Int =
    Iterator
      .iterate(grid)(applyRules(neighbors, limit))
      .sliding(2).find(seq => seq(0) == seq(1)) match {
      case Some(seq) => seq(0).findAll('#').size
      case None      => sys.error("illegal state")
    }

  private def applyRules(neighbors: Int => Array[Int], limit: Int)(grid: Grid[Char]): Grid[Char] =
    Grid1D.tabulate(grid.dim) { index =>
      grid(index) match {
        case '.'                                                         => '.'
        case 'L' if neighbors(index).count(p => grid(p) == '#') == 0     => '#'
        case '#' if neighbors(index).count(p => grid(p) == '#') >= limit => 'L'
        case _                                                           => grid(index)
      }
    }

  private def nextVisible(grid: Grid1D[Char], start: Int, dir: DirectionWithIndex): Option[Int] =
    Iterator
      .unfold(start)(p => grid.dim.moveS(p, dir).map(x => (x, x)))
      .find(p => grid(p) != '.')
}
