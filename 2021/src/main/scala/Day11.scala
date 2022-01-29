@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
object Day11 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = new Grid(input.map(_.toCharArray.map(_.toString.toInt)).toArray)
    Iterator.continually(flash(grid)).take(100).sum
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = new Grid(input.map(_.toCharArray.map(_.toString.toInt)).toArray)
    Iterator.continually(flash(grid)).zipWithIndex.find(_._1 == 100).get._2 + 1
  }

  def flash(g: Grid[Int]): Int = {
    g.indices.foreach { case p => g(p) = g(p) + 1 }

    while (g.indices.exists(g(_) > 9)) {
      g.indices.foreach { p =>
        if (g(p) > 9) {
          g(p) = -1
          g.neighbors(p).foreach { n =>
            if (g(n) != -1) {
              g(n) = g(n) + 1
            }
          }
        }
      }
    }

    var flash = 0
    g.indices.foreach { p =>
      if (g(p) == -1) {
        flash += 1
        g(p) = 0
      }
    }
    flash //.size
  }
}
