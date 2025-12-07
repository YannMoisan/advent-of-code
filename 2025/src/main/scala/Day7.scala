import com.yannmoisan.util.grid.{Grid, Grid1D, Pos}

object Day7 extends MultiPuzzle[Int, Long] {

  override def part1(input: Iterator[String]): Int = {
    val grid: Grid[Char] = Grid1D(input)

    var pos   = List(grid.dim.pos(grid.find('S').get))
    var count = 0
    (1 until grid.dim.height).foreach { _ =>
      pos = pos.flatMap { p =>
        val candidate = Pos(p.x, p.y + 1)
        if (grid(grid.dim.index(candidate)) == '^') {
          count += 1
          List(Pos(candidate.x - 1, candidate.y), Pos(candidate.x + 1, candidate.y))
        } else
          List(candidate)
      }.distinct
    }

    count
  }

  override def part2(input: Iterator[String]): Long = {
    val grid: Grid[Char] = Grid1D(input)

    var pos = Array.ofDim[Long](grid.dim.width)
    pos(grid.dim.pos(grid.find('S').get).x) = 1

    (1 until grid.dim.height).foreach { row =>
      val newPos = Array.ofDim[Long](grid.dim.width)
      pos.indices.foreach { i =>
        if (pos(i) != 0) {
          if (grid(grid.dim.index(Pos(i, row))) == '^') {
            newPos(i - 1) = newPos(i - 1) + pos(i)
            newPos(i + 1) = newPos(i + 1) + pos(i)
          } else {
            newPos(i) = newPos(i) + pos(i)
          }
        }
      }
      pos = newPos
    }

    pos.sum
  }
}
