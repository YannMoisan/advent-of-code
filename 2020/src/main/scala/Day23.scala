@SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.TraversableOps"))
object Day23 extends MultiPuzzle[Long, Long] {
  case class State(cups: IndexedSeq[Int], ptr: Int)

  override def part1(input: Iterator[String]): Long = {
    val state    = State("643719258".map(_.toString.toInt), 0)
    val end      = Iterator.iterate(state)(next).drop(100).next()
    val oneIndex = end.cups.indexOf(1)
    val indices  = (oneIndex + 1 until end.cups.length) ++ (0 until oneIndex)
    indices.map(end.cups).mkString.toLong
  }

  def next(s: State): State = {
    val currentCupLabel = s.cups(s.ptr)
    val pickup          = (s.ptr + 1 to s.ptr + 3).map(_ % s.cups.length)
    val pickUpValues    = pickup.map(s.cups)

    // find destination
    var dest   = -1
    var target = currentCupLabel - 1
    while (target >= 1 && dest == -1) {
      if (pickUpValues.contains(target)) {
        target = target - 1
      } else {
        dest = target
      }
    }
    if (dest == -1) {
      dest = (1 to s.cups.length).toSet.diff(pickup.map(s.cups).toSet).max
    }

    val tmp             = s.cups.filterNot(pickUpValues.contains)
    val destIndex       = tmp.indexOf(dest)
    val (before, after) = tmp.splitAt(destIndex + 1)
    val nextCups        = before ++ pickUpValues ++ after

    State(nextCups, (nextCups.indexOf(currentCupLabel) + 1) % s.cups.length)
  }

  override def part2(input: Iterator[String]): Long = {
    val state    = State("643719258".map(_.toString.toInt) ++ (10 to 1_000_000), 0)
    val end      = Iterator.iterate(state)(next).drop(100).next()
    val oneIndex = end.cups.indexOf(1)
    end.cups(oneIndex + 1).toLong * end.cups(oneIndex + 2).toLong
  }
}
