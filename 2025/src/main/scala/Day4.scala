import com.yannmoisan.util.grid.{Grid, Grid1D}

object Day4 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid: Grid[Char] = Grid1D(input)

    grid.dim.indices.count(p =>
      grid(p) == '@' && grid.dim.neighbors8(p).count(p2 => grid.apply(p2) == '@') < 4
    )
  }

  override def part2(input: Iterator[String]): Int = {
    val grid: Grid[Char] = Grid1D(input)

    var count    = 0
    var continue = true

    while (continue) {

      val toBeRemoved = grid.dim.indices.filter(p =>
        grid(p) == '@' && grid.dim.neighbors8(p).count(p2 => grid.apply(p2) == '@') < 4
      )
      count += toBeRemoved.length
      continue = toBeRemoved.length != 0
      toBeRemoved.foreach(p => grid(p) = '.')
    }
    count

  }
}
