@SuppressWarnings(
  Array("org.wartremover.warts.TraversableOps", "org.wartremover.warts.OptionPartial")
)
object Day1 extends SinglePuzzle[Int, Int] {
  override def part1(input: String): Int = floors(input).last

  override def part2(input: String): Int = floors(input).indexOf(-1)

  private def floors(input: String): Seq[Int] = input.scanLeft(0) {
    case (acc, i) => if (i == '(') acc + 1 else acc - 1
  }
}
