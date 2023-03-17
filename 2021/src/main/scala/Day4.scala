import com.yannmoisan.util.fp.loop
import com.yannmoisan.util.grid.{Grid, Grid1D, Pos}

// grids are represented as 2d mutable array
object Day4 extends MultiPuzzle[Int, Int] {
  case class State(grids: Array[Grid[Int]], numbers: Array[Int], index: Int)

  override def part1(input: Iterator[String]): Int = {
    val start  = parse(input)
    val end    = loop(start)(draw, _.grids.exists(hasCompleted))
    val winner = start.grids.find(hasCompleted)
    winner.fold(0)(sumOfAllNonMarkedNumbers) * start.numbers(end.index - 1)
  }

  override def part2(input: Iterator[String]): Int = {
    val start      = parse(input)
    val end        = loop(start)(draw, gs => gs.grids.count(hasCompleted) == gs.grids.length - 1)
    val lastWinner = start.grids.find(!hasCompleted(_))
    val end2       = loop(end)(draw, gs => gs.grids.count(hasCompleted) == gs.grids.length)
    lastWinner.fold(0)(sumOfAllNonMarkedNumbers) * start.numbers(end2.index - 1)
  }

  private def parse(input: Iterator[String]): State = {
    def parseGrid(arr: Array[String]): Grid[Int] =
      Grid1D((0 until 5).map(i => arr(i).trim.split("\\s+").map(_.toInt)).toArray)

    val lines                                 = input.toArray
    val numbers: Array[Int]                   = lines.head.split(",").map(_.toInt)
    val groupedLines: Iterator[Array[String]] = lines.drop(2).grouped(6)
    val grids: Array[Grid[Int]]               = groupedLines.map(parseGrid).toArray
    State(grids, numbers, 0)
  }

  private def hasCompleted(grid: Grid[Int]): Boolean = {
    def hasCompletedRow(grid: Grid[Int]): Boolean =
      (0 until 5).exists(y => (0 until 5).forall(x => grid(Pos(x, y)(grid.dim).index) == 0))

    def hasCompletedCol(grid: Grid[Int]): Boolean =
      (0 until 5).exists(x => (0 until 5).forall(y => grid(Pos(x, y)(grid.dim).index) == 0))

    hasCompletedRow(grid) || hasCompletedCol(grid)
  }

  private def sumOfAllNonMarkedNumbers(grid: Grid[Int]): Int =
    grid.dim.indices.foldLeft(0)((acc, p) => acc + grid(p))

  private def draw(state: State): State = {
    val value = state.numbers(state.index)
    state.grids.foreach(grid =>
      grid.dim.indices.filter(p => grid(p) == value).foreach(p => grid(p) = 0)
    )
    state.copy(index = state.index + 1)
  }
}
