object Day17 extends MultiPuzzle[Int, Int] {

  override def part1(input: Iterator[String]): Int = {
    val max = computeSolutions().maxBy(_._2)
    Iterator
      .iterate(State((0, 0), (max._1, max._2)))(next).takeWhile(_.velocity._2 >= 0).maxBy(_.pos._2).pos._2
  }

  override def part2(input: Iterator[String]): Int =
    computeSolutions().size

  def computeSolutions(): Seq[(Int, Int)] =
    for {
      x <- 1 to 176
      y <- -200 to 1200
      if hasReachedTheTarget(119, 176, -141, -84)(State((0, 0), (x, y)))
    } yield (x, y)

  case class State(pos: (Int, Int), velocity: (Int, Int))

  def hasReachedTheTarget(minX: Int, maxX: Int, minY: Int, maxY: Int)(init: State): Boolean =
    Iterator
      .iterate(init)(next).takeWhile(_.pos._2 >= minY).find { st =>
        st.pos._1 >= minX && st.pos._1 <= maxX && st.pos._2 >= minY && st.pos._2 <= maxY
      }.isDefined

  def next(s: State): State = {
    val newPos = (s.pos._1 + s.velocity._1, s.pos._2 + s.velocity._2)
    val newVel = (
      s.velocity._1 + (if (s.velocity._1 > 0) -1
                       else if (s.velocity._1 < 0) 1
                       else 0),
      s.velocity._2 - 1
    )
    State(newPos, newVel)
  }
}
