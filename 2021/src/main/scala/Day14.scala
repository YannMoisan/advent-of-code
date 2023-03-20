import scala.collection.mutable

object Day14 extends MultiPuzzle[Int, Long] {
  override def part1(input: Iterator[String]): Int =
    compute(input, 10).toInt

  override def part2(input: Iterator[String]): Long =
    compute(input, 40)

  def compute(input: Iterator[String], steps: Int): Long = {
    val lines                       = input.toArray
    val start                       = lines(0)
    val parsed: Map[String, String] = parse(lines.drop(2))

    val start0 = start.sliding(2).toArray.groupBy(identity).view.mapValues(_.length.toLong).toMap
    val res    = (1 to steps).foldLeft(start0) { case (acc, _) => next2(acc, parsed) }

    val m = mutable.Map[String, Long]()
    res.foreach { s =>
      val _ = m.updateWith(s"${s._1(0)}")(v => Some(v.getOrElse(0L) + s._2))
      val _ = m.updateWith(s"${s._1(1)}")(v => Some(v.getOrElse(0L) + s._2))
    }
    m(start.head.toString) = m(start.head.toString) + 1L
    m(start.last.toString) = m(start.last.toString) + 1L

    val freq = m.view.mapValues(_ / 2).toMap

    (freq.maxBy(_._2)._2) - (freq.minBy(_._2)._2)
  }

  def next2(cur: Map[String, Long], mapping: Map[String, String]): Map[String, Long] = {
    val m = mutable.Map[String, Long]()
    cur.foreach { s =>
      val ch = mapping(s._1)
      val _  = m.updateWith(s"${s._1(0)}$ch")(v => Some(v.getOrElse(0L) + s._2))
      val _  = m.updateWith(s"$ch${s._1(1)}")(v => Some(v.getOrElse(0L) + s._2))
    }
    m.toMap
  }

  def parse(lines: Array[String]): Map[String, String] =
    lines.map { line =>
      val Array(from, to) = line.split(" -> ")
      (from, to)
    }.toMap
}
