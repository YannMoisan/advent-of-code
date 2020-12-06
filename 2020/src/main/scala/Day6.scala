import scala.collection.mutable.ListBuffer

object Day6 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int =
    run(input, _.union(_))

  override def part2(input: Iterator[String]): Int =
    run(input, _.intersect(_))

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  private def run(input: Iterator[String], op: (Set[Char], Set[Char]) => Set[Char]): Int =
    split(input.toList, "")
      .map(lines => lines.map(_.toSet).reduce(op).size)
      .sum

  private def split(lines: List[String], sep: String): List[List[String]] = {
    val parent = new ListBuffer[List[String]]
    val child  = new ListBuffer[String]
    lines.foreach { line =>
      if (line != sep) {
        val _ = child.addOne(line)
      } else {
        val _ = parent.addOne(child.toList)
        child.clear()
      }
    }
    val _ = parent.addOne(child.toList)
    parent.toList
  }

}
