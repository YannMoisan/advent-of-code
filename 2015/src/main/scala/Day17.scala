// combination
// Learning : combination doestn't work with duplicates
object Day17 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = combinations(input).length

  override def part2(input: Iterator[String]): Int = {
    val comb      = combinations(input)
    val minLength = comb.minBy(_.size).size
    comb.count(_.size == minLength)
  }

  private def combinations(input: Iterator[String]): List[Set[Int]] = {
    val containers = input.map(_.toInt).toArray
    (0 until containers.length).toSet
      .subsets()
      .filter(indices => indices.foldLeft(0) { case (acc, i) => acc + containers(i) } == 150)
      .toList
  }
}
