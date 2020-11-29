object Day2 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = input.map(paper).sum

  override def part2(input: Iterator[String]): Int = input.map(ribbon).sum

  def paper(s: String): Int = {
    val arr = s.split('x').map(_.toInt).sorted
    val squareFeet = 2 * arr(0) * arr(1) + 2 * arr(0) * arr(2) + 2 * arr(1) * arr(
      2
    )
    val bonus = arr(0) * arr(1)
    squareFeet + bonus
  }

  def ribbon(s: String): Int = {
    val arr = s.split('x').map(_.toInt).sorted
    2 * (arr(0) + arr(1)) + arr(0) * arr(1) * arr(2)
  }
}
