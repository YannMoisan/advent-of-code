case class Pos(x: Int, y: Int)
@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day14 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = makeGrid(input)
    fill(grid)
    countSand(grid)
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = makeGrid(input)
    // add floor
    grid.head.indices.foreach(x => grid(grid.length - 1)(x) = '#')
    fill(grid)
    countSand(grid)
  }

  private def fill(grid: Array[Array[Char]]) = {
    def nextPos(grid: Array[Array[Char]], p: Pos): Option[Pos] =
      Seq(
        Pos(p.x, p.y + 1),     // one step down ?
        Pos(p.x - 1, p.y + 1), // one step down and to the left
        Pos(p.x + 1, p.y + 1)  // one step down and to the right
      ).find(p => grid(p.y)(p.x) == '.')

    var sand    = Pos(500, 0)
    var blocked = false
    while (!blocked && sand.y < grid.length - 1) {
      nextPos(grid, sand) match {
        case Some(p) => sand = p
        case None =>
          if (sand == Pos(500, 0))
            blocked = true
          grid(sand.y)(sand.x) = 'o'
          sand = Pos(500, 0)
      }
    }
  }

  private def makeGrid(input: Iterator[String]): Array[Array[Char]] = {
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
    val grid: Array[Array[Char]] = Array.fill[Char](maxY + 3, 1000)('.') // (y,x) coordinates

    // add paths on the grid
    paths.foreach { path =>
      path.sliding(2).foreach {
        case Array(start, end) => allPositions(start, end).foreach(pos => grid(pos.y)(pos.x) = '#')
      }
    }
    grid
  }

  private def countSand(grid: Array[Array[Char]]): Int =
    (for {
      x <- grid.head.indices
      y <- grid.indices
      if grid(y)(x) == 'o'
    } yield (x, y)).size
}
