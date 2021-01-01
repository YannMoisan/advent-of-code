import scala.collection.mutable

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day22 extends MultiPuzzle[Int, Int] {
  case class State(p1: List[Int], p2: List[Int])

  override def part1(input: Iterator[String]): Int = {
    val player1 = input.takeWhile(_ != "").toList.tail.map(_.toInt)
    val player2 = input.toList.tail.map(_.toInt)
    val init    = State(player1, player2)

    Iterator.iterate(init)(next).find(s => s.p1.isEmpty || s.p2.isEmpty) match {
      case Some(state) =>
        val winner = if (state.p1.isEmpty) state.p2 else state.p1
        winner.zip(winner.length to 1 by -1).map { case (a, b) => a * b }.sum
      case None => sys.error("illegal state")
    }
  }

  def next(s: State): State =
    if (s.p1.head > s.p2.head)
      State(s.p1.tail ++ Seq(s.p1.head, s.p2.head), s.p2.tail)
    else
      State(s.p1.tail, s.p2.tail ++ Seq(s.p2.head, s.p1.head))

  override def part2(input: Iterator[String]): Int = {
    val player1 = input.takeWhile(_ != "").toList.tail.map(_.toInt)
    val player2 = input.toList.tail.map(_.toInt)
    val init    = State(player1, player2)

    val winner = play2(init)._2
    winner.zip(winner.length to 1 by -1).map { case (a, b) => a * b }.sum
  }

  // play a game or a sub game
  def play2(s: State): (Int, List[Int]) = {

    // TODO refactor with Iterator
    var current                  = s
    val visited                  = mutable.Set[State]()
    val _                        = visited.add(current)
    var winner: (Int, List[Int]) = (-1, Nil)
    while (winner._1 == -1) {
      val next = next2(current)
      if (visited.contains(next)) {
        winner = (1, Nil)
      }
      if (next.p1.isEmpty) {
        winner = (2, next.p2)
      }

      if (next.p2.isEmpty) {
        winner = (1, next.p1)
      }
      val _ = visited.add(next)
      current = next
    }
    winner
  }

  def next2(s: State): State =
    // If both players have at least as many cards remaining in their deck as the value of the card they just drew,
    // the winner of the round is determined by playing a new game of Recursive Combat (see below).
    if (s.p1.tail.size >= s.p1.head && s.p2.tail.size >= s.p2.head) {
      if (play2(State(s.p1.tail.take(s.p1.head), s.p2.tail.take(s.p2.head)))._1 == 1) {
        State(s.p1.tail ++ Seq(s.p1.head, s.p2.head), s.p2.tail)
      } else {
        State(s.p1.tail, s.p2.tail ++ Seq(s.p2.head, s.p1.head))
      }
    } else {
      if (s.p1.head > s.p2.head)
        State(s.p1.tail ++ Seq(s.p1.head, s.p2.head), s.p2.tail)
      else
        State(s.p1.tail, s.p2.tail ++ Seq(s.p2.head, s.p1.head))
    }
}
