import com.yannmoisan.util.grid.{Grid, Grid1D, Pos}

object Day18 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid: Grid[Char] = Grid1D(input)
    val end              = (1 to 100).foldLeft(grid) { case (acc, _) => next(acc) }
    end.count(_ == '#')
  }

  override def part2(input: Iterator[String]): Int = {
    val grid: Grid[Char] = Grid1D(input)
    val coins =
      List(Pos(0, 0)(grid.dim), Pos(0, 99)(grid.dim), Pos(99, 0)(grid.dim), Pos(99, 99)(grid.dim))
    coins.foreach(p => grid(p.index) = '#')

    val end = (1 to 100).foldLeft(grid) {
      case (acc, _) =>
        val res = next(acc)
        coins.foreach(p => res(p.index) = '#')
        res
    }

    end.count(_ == '#')
  }

  def next(grid: Grid[Char]): Grid[Char] =
    Grid1D.tabulate(grid.dim) { index =>
      val neighbors   = grid.dim.neighbors8(index)
      var neighborsOn = 0
      var i           = 0
      while (i < neighbors.length) {
        val index = neighbors(i)
        if (grid(index) == '#') neighborsOn += 1
        i += 1
      }
      (grid(index), neighborsOn) match {
        case ('#', 2) | ('#', 3) => '#'
        case ('#', _)            => '.'
        case ('.', 3)            => '#'
        case ('.', _)            => '.'
      }
    }
}
