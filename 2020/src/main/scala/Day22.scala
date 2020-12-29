@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
object Day22 extends MultiPuzzle[Int, Int] {
  case class State(p1: List[Int], p2: List[Int])

  override def part1(input: Iterator[String]): Int = {
    val player1 = input.takeWhile(_ != "").toList.tail.map(_.toInt)
    val player2 = input.toList.tail.map(_.toInt)
    val init    = State(player1, player2)
//    val init = State(
//      List(9, 2, 6, 3, 1),
//      List(5, 8, 4, 7, 10)
//    )
    val tmp: Int = Iterator.iterate(init)(next).find(s => s.p1.isEmpty || s.p2.isEmpty) match {
      case Some(state) =>
        val winner = if (state.p1.isEmpty) state.p2 else state.p1
        winner.zip(winner.length to 1 by -1).map { case (a, b) => a * b }.sum
      case None => sys.error("illegal state")
    }

    println(tmp)
    42
  }

  def next(s: State): State =
    if (s.p1.head > s.p2.head)
      State(s.p1.tail ++ Seq(s.p1.head, s.p2.head), s.p2.tail)
    else
      State(s.p1.tail, s.p2.tail ++ Seq(s.p2.head, s.p1.head))

  override def part2(input: Iterator[String]): Int = 43
}
