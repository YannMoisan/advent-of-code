import com.yannmoisan.util.grid.{BFS, Grid, Grid1D, Pos}

object Day12 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = Grid1D(input)
    val s    = grid.find('S').get
    val e    = grid.find('E').get
    length(grid, s, e)
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = Grid1D(input)
    val e    = grid.find('E').get
    (0 to 40).map(y => Pos(0, y)).map(ss => length(grid, grid.dim.index(ss), e)).min
  }

  private def length(grid: Grid[Char], s: Int, e: Int): Int = {
    grid(s) = 'a' // hacky
    grid(e) = 'z' // hacky
    BFS.shortestPath(grid, s, 'z', (from, to) => grid(to) - grid(from) <= 1).get.length + 1
  }
}
