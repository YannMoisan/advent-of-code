
object Day2 extends MultiPuzzle[String, String] {

  trait Direction

  case object North extends Direction

  case object South extends Direction

  case object West extends Direction

  case object East extends Direction

  case class Pos(x: Int, y: Int)

  def parse(c: Char) = c match {
    case 'U' => North
    case 'D' => South
    case 'L' => West
    case 'R' => East
  }

  val keypad1 = Seq("123", "456", "789")
  val keypad2 = Seq("  1  ", " 234 ", "56789", " ABC ", "  D  ")

  def move(p: Pos, d: Direction): Pos = {
    val (dx, dy) = d match {
      case North => (0, -1)
      case South => (0, 1)
      case West => (-1, 0)
      case East => (1, 0)
    }
    Pos(p.x + dx, p.y + dy)
  }

  def isValid(k: Seq[String])(p: Pos) =
    p.x >= 0 && p.x < k(0).length && p.y >= 0 && p.y < k.length && k(p.y)(p.x) != ' '

  def validMove(keypad: Seq[String])(d: Direction) : Pos => Pos = f(move(_, d), isValid(keypad))

  def f[A](g: A => A, pred: A => Boolean) : A => A = {
    a =>
      val newA = g(a)
      if (pred(newA)) newA else a
  }

  def move(init: Pos, dirs: Seq[Direction], keypad: Seq[String]) = dirs.foldLeft(init){case (acc, i) => validMove(keypad)(i)(acc)}

  def part1(lines: Iterator[String]) : String = part(Pos(1, 1), keypad1)(lines)

  def part2(lines: Iterator[String]): String = part(Pos(2, 2), keypad2)(lines)

  def part(init: Pos, keypad: Seq[String]) = { lines: Iterator[String] =>
    val parsed: Iterator[IndexedSeq[Direction]] = lines.map(_.map(parse))
    val all = parsed.scanLeft(init) { case (acc, dirs) => move(acc, dirs, keypad) }.toList
    all.tail.map(p => keypad(p.y)(p.x)).mkString("")
  }
}
