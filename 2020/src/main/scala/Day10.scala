import scala.collection.mutable

object Day10 extends MultiPuzzle[Int, Long] {
  override def part1(input: Iterator[String]): Int = {
    val l                            = (0 :: input.map(_.toInt).toList).sorted
    val diffs                        = l.sliding(2).collect { case List(a, b) => b - a }.toList
    val grouped: Map[Int, List[Int]] = diffs.groupBy(identity)
    grouped(1).size * (grouped(3).size + 1)
  }

  override def part2(input: Iterator[String]): Long = {
    val l  = (0 :: input.map(_.toInt).toList).sorted
    val l2 = l :+ (l.last + 3)

    val r     = removables(l2.toBuffer).toArray
    var ret   = 1L
    var count = 1
    var i     = 1
    while (i < r.length) {
      if (r(i - 1) + 1 == r(i)) {
        count += 1
      }
      if (r(i - 1) + 1 != r(i) || i == r.length - 1) {
        ret *= (count match {
          case 1 => 2
          case 2 => 4
          case 3 => 7
        })
        count = 1
      }
      i += 1
    }
    ret
  }

  def removables(l: mutable.Buffer[Int]): Seq[Int] =
    (1 until l.length - 1).filter(i => l(i + 1) - l(i - 1) <= 3)

}
