import Day10.{next, State}

object Day10 extends SinglePuzzle[Int, String] {
  final case class State(list: IndexedSeq[Int], pos: Int, skipSize: Int)

  val state0 = State(0 to 255, 0, 0)

  def next(length: Int, s: State): State =
    State(
      reverse(s.list, s.pos, length),
      (s.pos + length + s.skipSize) % s.list.size,
      s.skipSize + 1
    )

  def reverse[A](s: IndexedSeq[A], from: Int, length: Int): IndexedSeq[A] =
    (0 until length / 2).foldLeft(s) {
      case (s, i) =>
        val low  = (from + i) % s.size
        val high = (from + length - 1 - i) % s.size
        val tmp  = s(low)
        s.updated(low, s(high))
          .updated(high, tmp)
    }

  override def part1(line: String): Int = {
    val stateF =
      line.split(",").map(_.toInt).foldLeft(state0) {
        case (s, i) => next(i, s)
      }
    stateF.list(0) * stateF.list(1)
  }

  override def part2(line: String): String =
    KnotHash.hash(line)
}

object KnotHash {
  def hash(s: String): String = {
    val state0 = State(0 to 255, 0, 0)

    val lengths = s.map(_.toInt) ++ Seq(17, 31, 73, 47, 23)
    val lengths64 = (0 until 63).foldLeft(lengths) {
      case (l, _) => l ++ lengths
    }
    val stateF =
      lengths64.foldLeft(state0) {
        case (s, i) => next(i, s)
      }
    val denseHash: Iterator[Int] =
      stateF.list.grouped(16).flatMap(_.reduceOption(_ ^ _).toList)
    denseHash.map(s => String.format("%2s", s.toHexString).replace(' ', '0')).mkString
    // Because each number in your dense hash will be between 0 and 255 (inclusive),
    // always represent each number as two hexadecimal digits (including a leading zero as necessary)
  }

}
