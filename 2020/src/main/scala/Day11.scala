object Day11 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = Grid(input.toArray.map(_.toArray))
    run(grid, pos => grid.neighbors(pos), 4)
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = Grid(input.toArray.map(_.toArray))
    run(grid, pos => Direction.all.flatMap(dir => nextVisible(grid, pos, dir)), 5)
  }

  private def run(grid: Grid, neighbors: Position => Seq[Position], limit: Int): Int = {
    val it = Iterator.iterate(grid)(applyRules(_, neighbors, limit))
    findFirstDuplicate(it) match {
      case Some(grid) => grid.count(_ == '#')
      case None       => sys.error("illegal state")
    }
  }

  private def applyRules(grid: Grid, neighbors: Position => Seq[Position], limit: Int): Grid = {
    val newGrid = grid.fill('?')
    newGrid.positions.foreach { pos =>
      grid(pos) match {
        case '.'                                                       => newGrid(pos) = '.'
        case 'L' if neighbors(pos).count(p => grid(p) == '#') == 0     => newGrid(pos) = '#'
        case '#' if neighbors(pos).count(p => grid(p) == '#') >= limit => newGrid(pos) = 'L'
        case _                                                         => newGrid(pos) = grid(pos)
      }
    }
    newGrid
  }

  private def nextVisible(grid: Grid, start: Position, dir: (Int, Int)): Option[Position] =
    Iterator
      .unfold(start)(p => grid.move(p, dir).map(x => (x, x)))
      .find(p => grid(p) != '.')

  private def findFirstDuplicate[A](it: Iterator[A]): Option[A] = {
    val withPreviousIterator = new Iterator[(A, Option[A])] {
      var prev: Option[A] = None

      override def hasNext: Boolean = it.hasNext

      override def next(): (A, Option[A]) = {
        val next = it.next()
        val ret  = (next, prev)
        prev = Some(next)
        ret
      }
    }
    withPreviousIterator.find { case (next, prev) => prev.exists(_ == next) }.map(_._1)
  }
}

object Direction {
  val all = Seq((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
}
case class Position(x: Int, y: Int)
case class Grid(arr: Array[Array[Char]]) {
  private val rowCount = arr.length
  private val colCount = arr.head.length

  def apply(p: Position): Char           = arr(p.y)(p.x)
  def update(p: Position, c: Char): Unit = arr(p.y)(p.x) = c

  def count(p: Char => Boolean): Int = arr.map(_.count(p)).sum

  def fill(elem: Char): Grid = Grid(Array.fill(rowCount)(Array.fill(colCount)(elem)))

  def move(p: Position, dir: (Int, Int)): Option[Position] = {
    val newp = Position(p.x + dir._1, p.y + dir._2)
    if (newp.x >= 0 && newp.y >= 0 && newp.x < colCount && newp.y < rowCount) Some(newp) else None
  }

  def positions: Seq[Position] =
    for {
      i <- 0 until colCount
      j <- 0 until rowCount
    } yield {
      Position(i, j)
    }

  def neighbors(p: Position) =
    Direction.all
      .map(d => Position(p.x + d._1, p.y + d._2))
      .filter(p => p.x >= 0 && p.y >= 0 && p.x < colCount && p.y < rowCount)

  override def equals(obj: Any): Boolean =
    obj match {
      case other: Grid => positions.forall(p => apply(p) == other(p))
      case _           => false
    }
}
