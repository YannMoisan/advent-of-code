import com.yannmoisan.util.graph.BFS
import com.yannmoisan.util.grid.{Dimension, Direction4, Pos}

object Day17 extends SinglePuzzle[String, Int] {
  val opened = "bcdef"

  def possibleMoves(s: String): Seq[Direction4] =
    MD5
      .md5(s).take(4).map(c => opened.contains(c)).zip(
        Seq(Direction4.Up, Direction4.Down, Direction4.Left, Direction4.Right)
      ).filter(_._1).map(
        _._2
      )

  def dirToString(d: Direction4) = d match {
    case Direction4.Up    => "U"
    case Direction4.Down  => "D"
    case Direction4.Right => "R"
    case Direction4.Left  => "L"
  }

  def move(input: String)(s: State): Seq[State] =
    if (s.p == Dimension(4, 4).index(Pos(3, 3))) Seq.empty
    else
      possibleMoves(input + s.history.map(dirToString).mkString)
        .collect {
          case d if Dimension(4, 4).moveS(s.p, d).isDefined =>
            State(Dimension(4, 4).moveS(s.p, d).get, s.history :+ d)
        }

  // TODO State S => (S, Direction4)
  case class State(p: Int, history: Seq[Direction4])

  override def part1(s: String): String = {
    val init  = State(Dimension(4, 4).index(Pos(0, 0)), Seq())
    val nodes = BFS.breadth_first_traverse(init, move("qzthpkfp"))
    nodes.find(_._1.p == Dimension(4, 4).index(Pos(3, 3))).get._1.history.map(dirToString).mkString
  }

  override def part2(s: String): Int = {
    val init  = State(Dimension(4, 4).index(Pos(0, 0)), Seq())
    val nodes = BFS.breadth_first_traverse(init, move("qzthpkfp"))
    val index = nodes.lastIndexWhere(_._1.p == Dimension(4, 4).index(Pos(3, 3)))
    nodes(index)._1.history.length
  }
}
