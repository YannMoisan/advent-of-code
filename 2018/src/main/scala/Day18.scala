import com.yannmoisan.util.grid.{Grid, Grid1D}

object Day18 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid: Grid[Char] = Grid1D(input)
    val end              = Iterator.iterate(grid)(next).drop(10).next()
    end.count(_ == '#') * end.count(_ == '|')
  }

  override def part2(input: Iterator[String]): Int =
    43

  /*
   *
      An open acre will become filled with trees if three or more adjacent acres contained trees. Otherwise, nothing happens.
      An acre filled with trees will become a lumberyard if three or more adjacent acres were lumberyards. Otherwise, nothing happens.
      An acre containing a lumberyard will remain a lumberyard if it was adjacent to at least one other lumberyard and at least one acre containing trees. Otherwise, it becomes open.
   */
  private def next(grid: Grid[Char]): Grid[Char] =
    Grid1D.tabulate(grid.dim) { i =>
      val n8 = grid.dim.neighbors8(i)
      grid(i) match {
        case '.' if n8.count(j => grid(j) == '|') >= 3                                       => '|'
        case '.'                                                                             => '.'
        case '|' if n8.count(j => grid(j) == '#') >= 3                                       => '#'
        case '|'                                                                             => '|'
        case '#' if n8.count(j => grid(j) == '#') >= 1 && n8.count(j => grid(j) == '|') >= 1 => '#'
        case '#'                                                                             => '.'
      }
    }
}
