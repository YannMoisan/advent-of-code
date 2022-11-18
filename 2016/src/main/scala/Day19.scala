
object Day19 extends SinglePuzzle[Int, Int] {

  class Node(var i: Int, var prev: Node, var next: Node) {
    def remove() = {
      prev.next = next
      next.prev = prev
    }
  }

  case class State(l: List[Int])

  def next(s: State): State = {
    val isOdd = s.l.size % 2 == 1
    val newL = s.l.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
    val newL2 = if (isOdd) newL.tail else newL
    State(newL2)
  }

  override def part2(s: String) : Int = {
    val n = 3014603

    val all = (0 until n).map(i => new Node(i, null, null))
    (0 until n).map { i =>
      all(i).prev = all((n + i - 1) % n)
      all(i).next = all((i + 1) % n)
    }

    var start = all(0)
    var mid = all(n / 2)

    (0 to (n - 1)).foreach { i =>
      mid.remove()
      mid = mid.next
      if ((n - i) % 2 == 1) mid = mid.next
      start = start.next
    }

    start.i + 1
  }

  override def part1(s: String) : Int = {
    val init = State((1 to 3014603).toList)
    val last = Rec.loop(init)(next, _.l.size == 1)
    last.l.head
  }
}
