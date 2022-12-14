case class Pos(x: Int, y: Int)

class Grid(private val grid: Array[Array[Char]]) {
  def height = grid.length
  def xindices: Seq[Int] = grid.head.indices
  def yindices: Seq[Int] = grid.indices
  def apply(p: Pos) : Char = grid(p.y)(p.x)
  def update(p: Pos, c: Char): Unit = grid(p.y)(p.x) = c
  def count(p: Char => Boolean) : Int = allPos.count(pos => p(apply(pos)))
  private def allPos: Seq[Pos] = for {
    x <- xindices
    y <- yindices
  } yield Pos(x, y)
}

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day14 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = makeGrid(input)
    fill(grid)
    grid.count(_ == 'o')
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = makeGrid(input)
    // add floor
    grid.xindices.foreach(x => grid(Pos(x, grid.height - 1)) = '#')
    fill(grid)
    grid.count(_ == 'o')
  }

  private def fill(grid: Grid) = {
    def nextPos(p: Pos): Option[Pos] =
      Seq(
        Pos(p.x, p.y + 1),     // one step down ?
        Pos(p.x - 1, p.y + 1), // one step down and to the left
        Pos(p.x + 1, p.y + 1)  // one step down and to the right
      ).find(p => grid(p) == '.')

    var sand    = Pos(500, 0)
    var blocked = false
    while (!blocked && sand.y < grid.height - 1) {
      nextPos(sand) match {
        case Some(p) => sand = p
        case None =>
          if (sand == Pos(500, 0))
            blocked = true
          grid(sand) = 'o'
          sand = Pos(500, 0)
      }
    }
  }

  private def makeGrid(input: Iterator[String]): Grid = {
    def parse(s: String): Array[Pos] =
      s.split(" -> ").map { seg =>
        val Array(x, y) = seg.split(",").map(_.toInt)
        Pos(x, y)
      }

    def allPositions(start: Pos, end: Pos): Seq[Pos] =
      if (start.x == end.x) {
        (start.y to end.y by math.signum(end.y - start.y)).map(y => Pos(start.x, y))
      } else {
        (start.x to end.x by math.signum(end.x - start.x)).map(x => Pos(x, start.y))
      }

    val paths: Seq[Array[Pos]]   = input.map(parse).toList
    val maxY                     = paths.flatten.maxBy(t => t.y).y
    val grid = new Grid(Array.fill[Char](maxY + 3, 1000)('.')) // (y,x) coordinates

    // add paths on the grid
    paths.foreach { path =>
      path.sliding(2).foreach {
        case Array(start, end) => allPositions(start, end).foreach(pos => grid(pos) = '#')
      }
    }
    grid
  }
}
