import com.yannmoisan.util.grid.{Direction, Grid, Grid1D, Pos}

object Day2 extends MultiPuzzle[String, String] {

  def parse(c: Char) = c match {
    case 'U' => Direction.Up
    case 'D' => Direction.Down
    case 'L' => Direction.Left
    case 'R' => Direction.Right
  }

  val keypad1 = Grid1D(Array("123", "456", "789"))
  val keypad2 = Grid1D(Array("  1  ", " 234 ", "56789", " ABC ", "  D  "))

  def validMove(keypad: Grid[Char])(d: Direction): Int => Int =
    from => keypad.dim.moveS(from, d).filter(keypad(_) != ' ').getOrElse(from)

  def move(init: Int, dirs: Seq[Direction], keypad: Grid[Char]) = dirs.foldLeft(init) {
    case (acc, i) => validMove(keypad)(i)(acc)
  }

  def part1(lines: Iterator[String]): String = part(Pos(1, 1)(keypad1.dim).index, keypad1)(lines)

  def part2(lines: Iterator[String]): String = part(Pos(2, 2)(keypad2.dim).index, keypad2)(lines)

  def part(init: Int, keypad: Grid[Char]) = { lines: Iterator[String] =>
    val parsed: Iterator[IndexedSeq[Direction]] = lines.map(_.map(parse))
    val all                                     = parsed.scanLeft(init) { case (acc, dirs) => move(acc, dirs, keypad) }.toList
    all.tail.map(p => keypad(p)).mkString("")
  }
}
