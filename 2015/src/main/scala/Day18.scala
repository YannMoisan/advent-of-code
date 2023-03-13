import com.yannmoisan.util.grid.{Grid, Grid1D, Pos}

object Day18 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid: Grid[Char] = Grid1D(input.toArray)
    val end = (1 to 100).foldLeft(grid) { case (acc, _) => next(acc) }
    end.count(_ == '#')
  }

  override def part2(input: Iterator[String]): Int = {
    val grid: Grid[Char] = Grid1D(input.toArray)
    grid(Pos(0,0)(grid.dim)) = '#'
    grid(Pos(0,99)(grid.dim)) = '#'
    grid(Pos(99,0)(grid.dim)) = '#'
    grid(Pos(99,99)(grid.dim)) = '#'

    val end = (1 to 100).foldLeft(grid) {
      case (acc, _) =>
        val res = next(acc)
        res(Pos(0, 0)(grid.dim)) = '#'
        res(Pos(0, 99)(grid.dim)) = '#'
        res(Pos(99, 0)(grid.dim)) = '#'
        res(Pos(99, 99)(grid.dim)) = '#'
        res
    }

    end.count(_ == '#')
  }

  def next(grid: Grid[Char]): Grid[Char] = {
    val res = Grid1D.fill(grid.dim.width, grid.dim.height)('?')

    grid.dim.allPos.foreach { p =>
      val neighbors = grid.dim.neighbors8(p)

      val neighborsOn = neighbors.count { p => grid(p) == '#' }
      val newState: Char = (grid(p), neighborsOn) match {
        case ('#', 2) | ('#', 3) => '#'
        case ('#', _) => '.'
        case ('.', 3) => '#'
        case ('.', _) => '.'
      }
      res(p) = newState
    }
    res
  }
}
