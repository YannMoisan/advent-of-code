import com.yannmoisan.util.grid.{Grid, Grid1D, Pos}

object Day22 extends MultiPuzzle[Int, Int] {

  override def part1(input: Iterator[String]): Int = {
    val s"depth: $depth_"              = input.next()
    val s"target: $targetx_,$targety_" = input.next()
    val depth                          = depth_.toInt
    val targetx                        = targetx_.toInt
    val targety                        = targety_.toInt

    val grid = makeGrid(depth, targetx, targety)
    riskLevel(grid)
  }

  private def makeGrid(depth: Int, targetx: Int, targety: Int): Grid[Int] = {
    val grid = Grid1D.fill(targetx + 1, targety + 1)(0)
    grid.dim.indices.foreach { i =>
      val geologicIndex = grid.dim.pos(i) match {
        case Pos(0, 0)                 => 0
        case Pos(`targetx`, `targety`) => 0 // stable identifier
        case Pos(targetx, 0)           => targetx * 16807
        case Pos(0, targety)           => targety * 48271
        case Pos(x, y) =>
          grid(Pos(x - 1, y)) * grid(Pos(x, y - 1))
      }
      val erosionLevel = (geologicIndex + depth) % 20183
      grid(i) = erosionLevel
    }
    grid
  }

  private def riskLevel(grid: Grid[Int]): Int = {
    var sum = 0
    grid.dim.indices.foreach(i => sum += grid(i) % 3)
    sum
  }

  override def part2(input: Iterator[String]): Int = 42
}
