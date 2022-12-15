case class Pos(x: Int, y: Int)

object Pos {
  def manhattan(a: Pos, b: Pos) : Int = math.abs(a.x - b.x) + math.abs(a.y - b.y)
}

case class Interval(inf: Int, sup: Int)

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
object Interval {
  def merge(a: Interval, b: Interval) : Seq[Interval] = {
    val (min, max) = if (a.inf < b.inf) (a, b) else (b, a)
    if (min.sup < max.inf) Seq(min,max)
    else if (max.sup < min.sup) Seq(min)
    else Seq(Interval(min.inf, max.sup))
  }

  def merge(l: Seq[Interval]) : Seq[Interval] = {
    val sorted = l.sortBy(_.inf)
    sorted.foldLeft(Seq[Interval]()){
      case (h :: t, int) => merge(h, int).reverse ++ t
      case (Nil, int) => Seq(int)
      case _ => throw new IllegalStateException()
    }
  }
}

object Day15 extends MultiPuzzle[Int, Long] {
  override def part1(input: Iterator[String]): Int = {
    val intervals = makeIntervals(input.toList, 2000000)
    val res = Interval.merge(intervals).map{ case Interval(inf, sup) => sup - inf + 1}.sum
    res - 1
  }

  private def makeIntervals(input: List[String], y: Int) = {
    input.flatMap {
      case s"Sensor at x=${sx}, y=${sy}: closest beacon is at x=${bx}, y=${by}" =>
        val s = Pos(sx.toInt, sy.toInt)
        val b = Pos(bx.toInt, by.toInt)
        val d = Pos.manhattan(s, b)
        val d2 = Pos.manhattan(s, Pos(s.x, y))
        if (d2 <= d) {
          val inf = s.x - (d - d2)
          val sup = s.x + (d - d2)
          Some(Interval(inf, sup))
        } else {
          None
        }
    }.toList
  }

  override def part2(input: Iterator[String]): Long = {
    val l = input.toList
    println(l.headOption)
    var foundX = -1
    var y = 0
    while (foundX == -1) {
      val intervals = makeIntervals(l, y)
      val res = Interval.merge(intervals)
      if (res.size > 1) {
        foundX = res(1).sup + 1
      } else {
        y += 1
      }
    }
    foundX.toLong * 4000000 + y
  }
}
