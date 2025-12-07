import scala.collection.mutable.ListBuffer

object Day5 extends MultiPuzzle[Int, Long] {

  override def part1(input: Iterator[String]): Int = {
    val (strIntervals, ids) = input.splitAt(187)
    val intervals: List[Interval] = strIntervals.map { s =>
      val Array(start, end) = s.split("-")
      Interval(start.toLong, end.toLong)
    }.toList

    ids.drop(1).map(_.toLong).count(id => intervals.exists(_.contains(id)))
  }

  override def part2(input: Iterator[String]): Long = {
    val (strIntervals, _) = input.splitAt(187)
    val intervals: List[Interval] = strIntervals.map { s =>
      val Array(start, end) = s.split("-")
      Interval(start.toLong, end.toLong)
    }.toList

    val sorted          = intervals.sortBy(_.start)
    val mergedIntervals = ListBuffer[Interval]()
    var cur             = sorted.head
    (1 until sorted.length).foreach { i =>
      val next = sorted(i)
      merge(cur, next) match {
        case List(_, _) =>
          mergedIntervals.addOne(cur)
          cur = next
        case List(merged) => cur = merged
        case _            =>
      }
    }
    mergedIntervals.addOne(cur)
    mergedIntervals.map(_.size).sum
  }

  private def merge(a: Interval, b: Interval): List[Interval] =
    if (b.start >= a.start && b.start <= a.end)
      List(Interval(a.start, Math.max(a.end, b.end)))
    else List(a, b)

  // represent an inclusive interval
  case class Interval(start: Long, end: Long) {
    def contains(value: Long): Boolean = value >= start && value <= end
    def size                           = end - start + 1
  }
}
