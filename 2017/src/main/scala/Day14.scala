import com.yannmoisan.util.grid.{BFS, Grid1D}

object Day14 extends SinglePuzzle[Int, Int] {
  override def part1(input: String): Int =
    List
      .tabulate(128)(i => s"$input-$i")
      .map(KnotHash.hash)
      .map(hexStringToBinaryString)
      .map(_.count(_ == '1'))
      .sum

  private def hexStringToBinaryString(hexString: String): String =
    hexString.map { s =>
      val binary = new java.math.BigInteger(s.toString, 16).toString(2)
      String.format("%4s", binary).replace(' ', '0')
    }.mkString

  override def part2(input: String): Int = {
    val arr = Array
      .tabulate(128)(i => s"$input-$i")
      .map(KnotHash.hash)
      .map(hexStringToBinaryString)
    val grid: Grid1D[Char] = Grid1D(arr.iterator)

    grid.dim.indices
      .collect {
        case i if grid(i) == '1' =>
          BFS.floodFill(grid, (_: Char, to: Char) => to == '1', i)
      }
      .distinct.size
  }
}
