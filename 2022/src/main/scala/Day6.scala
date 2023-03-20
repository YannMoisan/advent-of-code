object Day6 extends SinglePuzzle[Int, Int] {
  override def part1(input: String): Int =
    Iterator
      .from(3)
      .find(i => (i - 3 to i).map(input(_)).toSet.size == 4)
      .get + 1

  override def part2(input: String): Int =
    Iterator
      .from(13)
      .find(i => (i - 13 to i).map(input(_)).toSet.size == 14)
      .get + 1
}
