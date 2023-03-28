import com.yannmoisan.util.grid._

object Day2 extends MultiPuzzle[String, String] {

  def parse(c: Char) = c match {
    case 'U' => Direction4.Up
    case 'D' => Direction4.Down
    case 'L' => Direction4.Left
    case 'R' => Direction4.Right
  }

  val keypad1 = Grid1D(Iterator("123", "456", "789"))
  val keypad2 = Grid1D(Iterator("  1  ", " 234 ", "56789", " ABC ", "  D  "))

  def validMove(keypad: Grid[Char])(d: DirectionWithIndex): Int => Int =
    from => keypad.dim.moveS(from, d).filter(keypad(_) != ' ').getOrElse(from)

  def move(init: Int, dirs: Seq[DirectionWithIndex], keypad: Grid[Char]) = dirs.foldLeft(init) {
    case (acc, i) => validMove(keypad)(i)(acc)
  }

  def part1(lines: Iterator[String]): String = part(Pos(1, 1)(keypad1.dim).index, keypad1)(lines)

  def part2(lines: Iterator[String]): String = part(Pos(2, 2)(keypad2.dim).index, keypad2)(lines)

  def part(init: Int, keypad: Grid[Char]) = { lines: Iterator[String] =>
    val parsed: Iterator[IndexedSeq[DirectionWithIndex]] = lines.map(_.map(parse))
    val all                                              = parsed.scanLeft(init) { case (acc, dirs) => move(acc, dirs, keypad) }.toList
    all.tail.map(p => keypad(p)).mkString("")
  }
}
