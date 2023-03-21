import com.yannmoisan.util.geo.Position
import com.yannmoisan.util.grid.{Direction, Grid, Grid1D, Pos}

object Day3 extends MultiPuzzle[Long, Long] {
  override def part1(input: Iterator[String]): Long = {
    val grid = Grid1D(input)
    countTrees(grid, new Direction(3, 1, -1) {})
  }

  override def part2(input: Iterator[String]): Long = {
    val grid = Grid1D(input)
    List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
      .map { case (right, down) => countTrees(grid, new Direction(right, down, -1) {}) }
      .reduce(_ * _)
  }

  def countTrees(grid: Grid[Char], dir: Direction): Long =
    Iterator
      .iterate(Position(0, 0))(Position.move(_, dir))
      .takeWhile(_.y < grid.dim.height)
      .count(p => grid(Pos(p.x % grid.dim.width, p.y)(grid.dim).index) == '#')
      .toLong
}
