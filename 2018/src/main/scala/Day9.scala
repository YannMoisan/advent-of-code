import com.yannmoisan.util.collection.DoubleLinkedListNode

object Day9 extends SinglePuzzle[Long, Long] {
  // this impl is impure because the buffer is mutable and not part of the state
  case class State(node: DoubleLinkedListNode[Int], value: Int, player: Int, scores: Array[Long])

  override def part1(input: String): Long = {
    val s"$players players; last marble is worth $points points" = input
    highScore(players.toInt, points.toInt)
  }

  override def part2(input: String): Long = {
    val s"$players players; last marble is worth $points points" = input
    highScore(players.toInt, 100 * points.toInt)
  }

  def highScore(players: Int, points: Int): Long = {
    val buf   = DoubleLinkedListNode.single(0)
    val start = State(buf, 1, 0, Array.ofDim[Long](players))
    val end   = Iterator.iterate(start)(next).drop(points).next()
    end.scores.max
  }

  def next(s: State): State =
    if (s.value % 23 != 0) {
      s.node.next.addAfter(s.value)
      State(s.node.next.next, s.value + 1, (s.player + 1) % s.scores.length, s.scores)
    } else {
      val removedNode  = s.node.prev.prev.prev.prev.prev.prev.prev
      val removedValue = removedNode.remove()
      s.scores(s.player) += s.value + removedValue
      State(removedNode.next, s.value + 1, (s.player + 1) % s.scores.length, s.scores)
    }
}
