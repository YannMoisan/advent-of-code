@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
object Day24 extends MultiPuzzle[Long, Long] {
  override def part1(input: Iterator[String]): Long = common(input, 3)

  override def part2(input: Iterator[String]): Long = common(input, 4)

  private def common(input: Iterator[String], groupCount: Int): Long = {
    val total = input.toList.map(_.toLong)
    val target = total.sum / groupCount
    val count = (1 to total.length).find(i => (total.combinations(i).exists(_.sum == target)))
    total.combinations(count.get).filter(_.sum == target).minBy(_.product).product
  }
}
