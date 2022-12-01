import scala.collection.mutable.ListBuffer

object Day1 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = common(input, 1)

  override def part2(input: Iterator[String]): Int = common(input, 3)

  private def common(input: Iterator[String], k: Int): Int = split(input.toList, "")
    .map(_.map(_.toInt).sum)
    .sorted
    .takeRight(k)
    .sum

  private def split[A](l: List[A], sep: A) : List[List[A]] = {
    val buf = new ListBuffer[List[A]]()
    var current : List[A] = Nil
    val it = l.iterator
    while (it.hasNext) {
      val line = it.next()
      if (line == sep) {
        val _ = buf.append(current.reverse)
        current = Nil.asInstanceOf[List[A]]
      } else {
        current = line :: current
      }
    }
    val _ = buf.append(current.reverse)
    buf.toList
  }
}
