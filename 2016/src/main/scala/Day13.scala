import com.yannmoisan.util.grid.{Grid, Grid1D, Pos}

object Day13 extends SinglePuzzle[Int, Int] {

  def moves(grid: Grid[Char]): Int => Seq[Int] = { i =>
    grid.dim.neighbors4(i).filter(grid(_) == '.').toIndexedSeq
  }

  def generateMaze(w: Int, h: Int, fav: Int): Grid[Char] =
    Grid1D(Array.tabulate(w, h) { case (y, x) => isNumber(x.toLong, y.toLong, fav.toLong) })

  def isNumber(x: Long, y: Long, fav: Long): Char =
    if ((x * x + 3 * x + 2 * x * y + y + y * y + fav).toBinaryString.count(_ == '1') % 2 == 0) '.'
    else '#'

  override def part1(s: String): Int = {
    val grid: Grid[Char] = generateMaze(100, 100, 1350)
    val stream           = BFS.breadth_first_traverse(Pos(1, 1)(grid.dim).index, moves(grid))
    stream.find(_._1 == Pos(31, 39)(grid.dim).index).get._2.length - 1
  }

  override def part2(s: String): Int = {
    val grid: Grid[Char]  = generateMaze(100, 100, 1350)
    val stream            = BFS.breadth_first_traverse(Pos(1, 1)(grid.dim).index, moves(grid))
    val limitedStream     = stream.takeWhile(_._2.length <= 51)
    val visited: Set[Int] = limitedStream.map(_._1).toSet
    visited.size
  }

}
