import com.yannmoisan.util.grid.{Grid, Grid1D}

object Day11 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = Grid1D(input.map(_.toCharArray.map(_.toString.toInt)).toArray)
    Iterator.continually(flash(grid)).take(100).sum
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = Grid1D(input.map(_.toCharArray.map(_.toString.toInt)).toArray)
    Iterator.continually(flash(grid)).indexOf(100) + 1
  }

  def flash(g: Grid[Int]): Int = {
    g.dim.indices.foreach { case p => g(p) = g(p) + 1 }

    while (g.dim.indices.exists(p => g(p) > 9)) {
      g.dim.indices.foreach { p =>
        if (g(p) > 9) {
          g(p) = -1
          g.dim.neighbors8(p).foreach { n =>
            if (g(n) != -1) {
              g(n) = g(n) + 1
            }
          }
        }
      }
    }

    var flash = 0
    g.dim.indices.foreach { p =>
      if (g(p) == -1) {
        flash += 1
        g(p) = 0
      }
    }
    flash //.size
  }
}
