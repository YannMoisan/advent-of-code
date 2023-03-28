import com.yannmoisan.util.grid.{Grid, Grid1D, Pos}

object Day18 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid: Grid[Char] = Grid1D(input)
    val end              = Iterator.iterate(grid)(next).drop(100).next()
    end.count(_ == '#')
  }

  override def part2(input: Iterator[String]): Int = {
    val grid: Grid[Char] = Grid1D(input)
    val coins =
      List(Pos(0, 0), Pos(0, 99), Pos(99, 0), Pos(99, 99))
    coins.foreach(p => grid(p) = '#')

    val end = Iterator
      .iterate(grid) { g =>
        val res = next(g)
        coins.foreach(p => res(p) = '#')
        res
      }.drop(100).next()

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
