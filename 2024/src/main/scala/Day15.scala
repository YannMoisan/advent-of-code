import com.yannmoisan.util.grid.{Direction4, Grid1D}

object Day15 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val it: Iterator[String] = input.takeWhile(!_.isEmpty)
    val instr                = input.takeWhile(!_.isEmpty)
    val g                    = Grid1D(it)
    g.debug()
    instr.map(s => s.map(parse)).foreach(println)
    42
  }

  def parse(c: Char): Direction4 =
    c match {
      case '^' => Direction4.Up
      case 'v' => Direction4.Down
      case '<' => Direction4.Left
      case '>' => Direction4.Right
    }

  override def part2(input: Iterator[String]): Int = ???
}
