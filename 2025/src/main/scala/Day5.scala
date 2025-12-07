import scala.collection.mutable.ListBuffer

object Day5 extends MultiPuzzle[Int, Long] {

  override def part1(input: Iterator[String]): Int = {
    val (strIntervals, ids) = input.splitAt(187)
    val intervals: List[(Long, Long)] = strIntervals.map { s =>
      val Array(fst, snd) = s.split("-")
      (fst.toLong, snd.toLong)
    }.toList

    println(intervals.size)
    intervals.take(5).map(println)

    ids.drop(1).map(_.toLong).count { id =>
      intervals.exists(inter => inter._1 <= id && inter._2 >= id)
    }

  }

  override def part2(input: Iterator[String]): Long = {
    val (strIntervals, _) = input.splitAt(187)
    val intervals: List[(Long, Long)] = strIntervals.map { s =>
      val Array(fst, snd) = s.split("-")
      (fst.toLong, snd.toLong)
    }.toList

    val sorted = intervals.sortBy(_._1)
    val tmp    = ListBuffer[(Long, Long)]()
    var i      = 1
    var cur    = sorted.head
    while (i < sorted.length) {
      val next   = sorted(i)
      val merged = merge(cur, next)
      if (merged.size == 2) {
        tmp.addOne(cur)
        cur = next
      } else {
        cur = merged.head
      }
      i += 1
    }
    tmp.addOne(cur)
    tmp.map { case (a, b) => 1 + b - a }.sum

  }

  private def merge(a: (Long, Long), b: (Long, Long)): List[(Long, Long)] =
    if (b._1 >= a._1 && b._1 <= a._2)
      List((a._1, Math.max(a._2, b._2)))
    else List(a, b)
}
