object Day25 extends SinglePuzzle[Long, Int] {
  override def part1(input: String): Long = {
    val row    = 2947
    val column = 3029
    val limit  = index(row, column)
    Iterator.iterate(20151125L)(next).drop(limit - 1).next()
  }

  def next(v: Long): Long = (v * 252533) % 33554393

  // c + sum(1..r+c-2)i
  def index(row: Int, col: Int): Int = {
    val n = row + col - 2
    col + (n * (n + 1)) / 2
  }

  override def part2(input: String): Int = 42
}
