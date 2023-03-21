import com.yannmoisan.util.grid.{BFS, Grid1D}

object Day9 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = Grid1D(input.toArray.map(_.toCharArray.map(_.asDigit)))
    grid.dim.indices.map { i =>
      if (grid.dim.neighbors4(i).forall(j => grid(j) > grid(i)))
        grid(i) + 1
      else 0
    }.sum
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = Grid1D(input.toArray.map(_.toCharArray.map(_.asDigit)))
    val lowPoints = grid.dim.indices.filter { i =>
      grid.dim.neighbors4(i).forall(j => grid(j) > grid(i))
    }

    lowPoints.map(BFS.floodFill(grid, _)).sortBy(x => -x).take(3).product
  }

}
