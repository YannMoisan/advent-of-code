import scala.collection.mutable

object Day23 extends MultiPuzzle[Long, Long] {
  override def part1(input: Iterator[String]): Long = {
    val arr = "643719258".map(_.toString.toInt).toArray

    // use a Map to store a circular list.
    // each element represents a value (key) and its successor (value).
    val succ = mutable.Map[Int, Int]()
    (0 to arr.length - 2).foreach(i => succ(arr(i)) = arr(i + 1))
    succ(arr(arr.length - 1)) = arr(0)

    var ptr = arr(0)

    (1 to 100).foreach(_ => ptr = update2(succ, ptr, 9))

    // TODO recursion
    Seq(
      succ(1),
      succ(succ(1)),
      succ(succ(succ(1))),
      succ(succ(succ(succ(1)))),
      succ(succ(succ(succ(succ(1))))),
      succ(succ(succ(succ(succ(succ(1)))))),
      succ(succ(succ(succ(succ(succ(succ(1))))))),
      succ(succ(succ(succ(succ(succ(succ(succ(1))))))))
    ).mkString.toLong
  }

  def update2(m: mutable.Map[Int, Int], currentCupLabel: Int, max: Int): Int = {
    val a = m(currentCupLabel)
    val b = m(a)
    val c = m(b)

    // find destination
    var dest   = -1
    var target = currentCupLabel - 1
    while (target >= 1 && dest == -1)
      if (target == a || target == b || target == c) {
        target = target - 1
      } else {
        dest = target
      }
    if (dest == -1) {
      dest = (max - 2 to max).toSet.diff(Set(a, b, c)).max
    }

    m(currentCupLabel) = m(c)
    m(c) = m(dest)
    m(dest) = a
    m(currentCupLabel)
  }

  override def part2(input: Iterator[String]): Long = {
    val arr = ("643719258".map(_.toString.toInt) ++ (10 to 1_000_000)).toArray

    // use a Map to store a circular list.
    // each element represents a value (key) and its successor (value).
    val succ = mutable.Map[Int, Int]()
    (0 to arr.length - 2).foreach(i => succ(arr(i)) = arr(i + 1))
    succ(arr(arr.length - 1)) = arr(0)

    var ptr = arr(0)

    (1 to 10_000_000).foreach(_ => ptr = update2(succ, ptr, 1_000_000))

    succ(1).toLong * succ(succ(1)).toLong
  }
}
