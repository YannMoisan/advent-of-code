import scala.util.matching.Regex

object Day19 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val patterns = input.next().split(", ")
    val r        = new Regex(s"^(${patterns.mkString("|")})+$$")

    val rest = input.drop(1).toList
    rest.count(line => r.matches(line))
  }

  override def part2(input: Iterator[String]): Int = ???
}
