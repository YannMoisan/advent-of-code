import com.yannmoisan.util.grid.{BFS, Grid1D, Pos}

@SuppressWarnings(
  Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial")
)
object Day12 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = Grid1D(input.toArray)
    val s    = grid.find('S').get
    val e    = grid.find('E').get
    length(grid, s, e)
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = Grid1D(input.toArray)
    val e    = grid.find('E').get
    (0 to 40).map(y => Pos(0, y)(grid.dim)).map(ss => length(grid, ss, e)).min
  }

  private def length(grid: Grid1D[Char], s: Pos, e: Pos): Int = {
    grid(s) = 'a' // hacky
    grid(e) = 'z' // hacky
    BFS.shortestPath(grid, s, 'z', (from, to) => grid(to) - grid(from) <= 1).get.length + 1
  }
}
