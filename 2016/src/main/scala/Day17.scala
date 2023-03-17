object Day17 extends SinglePuzzle[String, Int] {
  trait Direction

  case object North extends Direction

  case object South extends Direction

  case object West extends Direction

  case object East extends Direction

  case class Pos(x: Int, y: Int)

  def move(p: Pos, d: Direction): Pos = {
    val (dx, dy) = d match {
      case North => (0, -1)
      case South => (0, 1)
      case West  => (-1, 0)
      case East  => (1, 0)
    }
    Pos(p.x + dx, p.y + dy)
  }

  def isValid(p: Pos) =
    p.x >= 0 && p.x < 4 && p.y >= 0 && p.y < 4

  val opened = "bcdef"

  def possibleMoves(s: String): Seq[Direction] =
    MD5
      .md5(s).take(4).map(c => opened.contains(c)).zip(Seq(North, South, West, East)).filter(_._1).map(
        _._2
      )

  def dirToString(d: Direction) = d match {
    case North => "U"
    case South => "D"
    case East  => "R"
    case West  => "L"
  }

  def move(input: String)(s: State): Seq[State] =
    if (s.p == Pos(3, 3)) Seq.empty
    else
      possibleMoves(input + s.history.map(dirToString).mkString)
        .map(d => State(move(s.p, d), s.history :+ d))
        .filter(s => isValid(s.p))

  // TODO State S => (S, Direction)
  case class State(p: Pos, history: Seq[Direction])

  override def part1(s: String): String = {
    val init  = State(Pos(0, 0), Seq())
    val nodes = BFS.breadth_first_traverse(init, move("qzthpkfp"))
    nodes.find(_._1.p == Pos(3, 3)).get._1.history.map(dirToString).mkString
  }

  override def part2(s: String): Int = {
    val init  = State(Pos(0, 0), Seq())
    val nodes = BFS.breadth_first_traverse(init, move("qzthpkfp"))
    val index = nodes.lastIndexWhere(_._1.p == Pos(3, 3))
    nodes(index)._1.history.length
  }
}
