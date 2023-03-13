import com.yannmoisan.util.grid.Grid1D
import com.yannmoisan.util.grid.Pos

// grids are represented as 2d mutable array
object Day4 extends MultiPuzzle[Int, Int] {
  case class State(grids: Array[Grid1D[Int]], numbers: Array[Int], index: Int)

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
    def parseGrid(arr: Array[String]): Grid1D[Int] =
      Grid1D((0 until 5).map(i => arr(i).trim.split("\\s+").map(_.toInt)).toArray)

    val lines                                 = input.toArray
    val numbers: Array[Int]                   = lines.head.split(",").map(_.toInt)
    val groupedLines: Iterator[Array[String]] = lines.drop(2).grouped(6)
    val grids: Array[Grid1D[Int]]               = groupedLines.map(parseGrid).toArray
    State(grids, numbers, 0)
  }

  private def hasCompleted(grid: Grid1D[Int]): Boolean = {
    def hasCompletedRow(grid: Grid1D[Int]): Boolean =
      (0 until 5).exists(y => (0 until 5).forall(x => grid(Pos(x,y)(grid.dim)) == 0))

    def hasCompletedCol(grid: Grid1D[Int]): Boolean =
      (0 until 5).exists(x => (0 until 5).forall(y => grid(Pos(x,y)(grid.dim)) == 0))

    hasCompletedRow(grid) || hasCompletedCol(grid)
  }

  private def sumOfAllNonMarkedNumbers(grid: Grid1D[Int]): Int =
    grid.dim.allPos.foldLeft(0)((acc, p) => acc + grid(p))

  private def draw(state: State): State = {
    val value = state.numbers(state.index)
    state.grids.foreach(grid => grid.dim.allPos.filter(grid(_) == value).foreach(grid(_) = 0))
    state.copy(index = state.index + 1)
  }

  private def loop[S](init: S)(f: S => S, isEnd: S => Boolean): S = {
    val next = f(init)
    if (isEnd(next)) next else loop(next)(f, isEnd)
  }
}
