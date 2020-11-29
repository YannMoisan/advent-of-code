import scala.annotation.tailrec
import scala.collection.mutable
object Day1 extends MultiPuzzle[Int, Int] {
  override def part1: Iterator[String] => Int = { iter =>
    iter.toArray.map(_.toInt).sum
  }

  /** Stream approach */
  override def part2: Iterator[String] => Int = part2_stream

  def part2_stream: Iterator[String] => Int = { iter =>
    val frequencies = iter.toArray.map(_.toInt)
    Stream
      .continually(frequencies)
      .flatten
      .scanLeft((0, Set.empty[Int])) {
        case ((sum, visited), freq) => (sum + freq, visited + sum)
      }
      .find { case (sum, visited) => visited.contains(sum)}
      .get._1
  }

  /** Recursive impl. */
  def part2_recursive: Iterator[String] => Int = { iter =>
    val lines = iter.toArray
    loop(lines.map(_.toInt), 0, 0, Set.empty)
  }

  @tailrec
  def loop(lines: Array[Int], idx: Int, sum: Int, visited: Set[Int]) : Int = {
    val v = lines(idx % lines.length)
    val sum2 = sum + v
    if (visited.contains(sum2)) sum2
    else loop(lines, idx + 1, sum2, visited + sum2)
  }

  /** imperative approach */
  def part2_imperative: Iterator[String] => Int = { iter =>
    val lines = iter.toArray
    var i = 0
    var sum = 0
    val visited = mutable.Set(0)
    var found: Option[Int] = None
    while (found.isEmpty) {
      sum += lines(i % lines.length).toInt
      if (visited.contains(sum)) { found = Some(sum) }
      visited += sum
      i += 1
    }
    found.get
  }
}
