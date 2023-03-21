import com.yannmoisan.util.collection.firstDuplicate

object Day1 extends MultiPuzzle[Int, Int] {
  override def part1(iter: Iterator[String]): Int =
    iter.map(_.toInt).sum

  /** LazyList approach */
  override def part2(iter: Iterator[String]): Int = {
    val frequencies = iter.map(_.toInt).toArray
    val it = Iterator
      .continually(frequencies)
      .flatten
      .scanLeft(0)(_ + _)
    firstDuplicate(it).get
  }
}
