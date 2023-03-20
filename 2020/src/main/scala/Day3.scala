object Day3 extends MultiPuzzle[Long, Long] {
  override def part1(input: Iterator[String]): Long = {
    val grid = input.toArray
    countTrees(grid, 3, 1)
  }

  override def part2(input: Iterator[String]): Long = {
    val grid = input.toArray
    List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
      .map { case (right, down) => countTrees(grid, right, down) }
      .reduce(_ * _)
  }

  def countTrees(grid: Array[String], right: Int, down: Int): Long =
    (0 until grid.length / down)
      .count(i => grid(i * down)(i * right % grid.head.length) == '#').toLong
}
