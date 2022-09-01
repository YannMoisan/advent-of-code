// label:regex, json
object Day12 extends SinglePuzzle[Int, Int] {
  // https://www.scala-lang.org/api/2.12.5/scala/util/matching/Regex.html
  override def part1(input: String): Int = {
    val numbers = raw"-?\d+".r
    numbers.findAllIn(input).toList.map(_.toInt).sum
  }

  override def part2(input: String): Int = 43
}
