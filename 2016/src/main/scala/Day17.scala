import com.yannmoisan.util.grid.{Dimension, Direction, Direction4, Pos}

object Day17 extends SinglePuzzle[String, Int] {
  val opened = "bcdef"

  def possibleMoves(s: String): Seq[Direction4] =
    MD5
      .md5(s).take(4).map(c => opened.contains(c)).zip(
        Seq(Direction.Up, Direction.Down, Direction.Left, Direction.Right)
      ).filter(_._1).map(
        _._2
      )

  def dirToString(d: Direction4) = d match {
    case Direction.Up    => "U"
    case Direction.Down  => "D"
    case Direction.Right => "R"
    case Direction.Left  => "L"
  }

  def move(input: String)(s: State): Seq[State] =
    if (s.p == Pos(3, 3)(Dimension(4, 4)).index) Seq.empty
    else
      possibleMoves(input + s.history.map(dirToString).mkString)
        .collect {
          case d if Dimension(4, 4).moveS(s.p, d).isDefined =>
            State(Dimension(4, 4).moveS(s.p, d).get, s.history :+ d)
        }

  // TODO State S => (S, Direction4)
  case class State(p: Int, history: Seq[Direction4])

  override def part1(s: String): String = {
    val init  = State(Pos(0, 0)(Dimension(4, 4)).index, Seq())
    val nodes = BFS.breadth_first_traverse(init, move("qzthpkfp"))
    nodes.find(_._1.p == Pos(3, 3)(Dimension(4, 4)).index).get._1.history.map(dirToString).mkString
  }

  override def part2(s: String): Int = {
    val init  = State(Pos(0, 0)(Dimension(4, 4)).index, Seq())
    val nodes = BFS.breadth_first_traverse(init, move("qzthpkfp"))
    val index = nodes.lastIndexWhere(_._1.p == Pos(3, 3)(Dimension(4, 4)).index)
    nodes(index)._1.history.length
  }
}
