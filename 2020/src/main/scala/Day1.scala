object Day1 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val numbers = input.map(_.toInt).toSet
    twoSum(numbers, 2020) match {
      case Some((a, b)) => a * b
      case None         => sys.error("illegal state")
    }
  }

  override def part2(input: Iterator[String]): Int = {
    val numbers = input.map(_.toInt).toSet

    numbers
      .flatMap(x => twoSum(numbers, 2020 - x).map { case (a, b) => (x, a, b) })
      .headOption match {
      case Some((a, b, c)) => a * b * c
      case None            => sys.error("illegal state")
    }
  }

  def twoSum(set: Set[Int], target: Int): Option[(Int, Int)] =
    set.find(x => set.contains(target - x)).map(x => (x, target - x))
}
