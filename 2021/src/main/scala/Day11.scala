import com.yannmoisan.util.grid.{Grid, Grid1D}

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
object Day11 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = Grid1D(input.map(_.toCharArray.map(_.toString.toInt)).toArray)
    Iterator.continually(flash(grid)).take(100).sum
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = Grid1D(input.map(_.toCharArray.map(_.toString.toInt)).toArray)
    Iterator.continually(flash(grid)).zipWithIndex.find(_._1 == 100).get._2 + 1
  }

  def flash(g: Grid[Int]): Int = {
    g.dim.allPos.foreach { case p => g(p.index) = g(p.index) + 1 }

    while (g.dim.allPos.exists(p => g(p.index) > 9)) {
      g.dim.allPos.foreach { p =>
        if (g(p.index) > 9) {
          g(p.index) = -1
          g.dim.neighbors8(p.index).foreach { n =>
            if (g(n) != -1) {
              g(n) = g(n) + 1
            }
          }
        }
      }
    }

    var flash = 0
    g.dim.allPos.foreach { p =>
      if (g(p.index) == -1) {
        flash += 1
        g(p.index) = 0
      }
    }
    flash //.size
  }
}
