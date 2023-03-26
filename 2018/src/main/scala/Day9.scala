import scala.collection.mutable.ArrayBuffer

object Day9 extends SinglePuzzle[Long, Long] {
  // this impl is impure because the buffer is mutable and not part of the state
  case class State(pos: Int, value: Int, player: Int, scores: Array[Long])

  override def part1(input: String): Long = {
    val s"$players players; last marble is worth $points points" = input
    highScore(players.toInt, points.toInt)
  }

  override def part2(input: String): Long = {
    val s"$players players; last marble is worth $points points" = input
    highScore(players.toInt, points.toInt)
  }

  def highScore(players: Int, points: Int): Long = {
    val buf   = ArrayBuffer(0)
    val start = State(0, 1, 0, Array.ofDim[Long](players))
    val end   = Iterator.iterate(start)(next(buf, _)).drop(points).next()
    end.scores.max
  }

  def next(buf: ArrayBuffer[Int], s: State): State =
    if (s.value % 23 != 0) {
      val newPos = (s.pos + 2) % (buf.size)
      buf.insert(newPos, s.value)
      State(newPos, s.value + 1, (s.player + 1) % s.scores.length, s.scores)
    } else {
      val newPos       = (s.pos - 7 + buf.size) % buf.size
      val removedValue = buf.remove(newPos)
      s.scores(s.player) += s.value + removedValue
      State(newPos, s.value + 1, (s.player + 1) % s.scores.length, s.scores)
    }

//  override def part2(input: String): Int = 42
}
