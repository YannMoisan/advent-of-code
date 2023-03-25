import com.yannmoisan.util.grid.{Dimension, Grid, Grid1D, Pos}

object Day11 extends SinglePuzzle[String, String] {
  override def part1(input: String): String = {
    val sn = input.toInt
    val grid = Grid1D.tabulate(Dimension(300, 300)) { i =>
      val p = Dimension(300, 300).pos(i)
      power(p.x + 1, p.y + 1, sn)
    }

    val largest = yo(grid: Grid[Int], Seq(3))
    s"${largest._1},${largest._2}"
  }

  override def part2(input: String): String = {
    val sn = input.toInt
    val grid = Grid1D.tabulate(Dimension(300, 300)) { i =>
      val p = Dimension(300, 300).pos(i)
      power(p.x + 1, p.y + 1, sn)
    }
    val largest = yo(grid, 1 to 300)
    s"$largest,${largest._2},${largest._3}"
  }

  def power(x: Int, y: Int, sn: Int): Int = {
    /*
     *
        Find the fuel cell's rack ID, which is its X coordinate plus 10.
        Begin with a power level of the rack ID times the Y coordinate.
        Increase the power level by the value of the grid serial number (your puzzle input).
        Set the power level to itself multiplied by the rack ID.
        Keep only the hundre- ds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
        Subtract 5 from the power level.
     * */
    val rackId       = x + 10
    val power        = rackId * y
    val incPower     = power + sn
    val mulPower     = incPower * rackId
    val hundredDigit = (mulPower / 100) % 10
    hundredDigit - 5

  }

  private def yo(grid: Grid[Int], sizes: Seq[Int]): (Int, Int, Int) = {
    val candidates = for {
      size <- sizes
      x    <- 1 to 300 - size + 1
      y    <- 1 to 300 - size + 1
    } yield {
      (x, y, size)
    }

    candidates
      .map { c =>
        val sum = (for {
          dx <- 0 until c._3
          dy <- 0 until c._3
        } yield {
          grid(Pos(c._1 + dx - 1, c._2 + dy - 1)(grid.dim).index)
        }).sum
        (c, sum)
      }.maxBy(_._2)._1

  }
}
