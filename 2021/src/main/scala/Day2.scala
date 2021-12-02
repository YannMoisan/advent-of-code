object Day2 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val (h, d) = input.foldLeft((0, 0)) {
      case ((h, d), instr) =>
        instr match {
          case s"forward $v" => (h + v.toInt, d)
          case s"down $v"    => (h, d + v.toInt)
          case s"up $v"      => (h, d - v.toInt)
        }
    }
    h * d
  }

  override def part2(input: Iterator[String]): Int = {
    val (_, h, d) = input.foldLeft((0, 0, 0)) {
      case ((a, h, d), instr) =>
        instr match {
          case s"forward $v" => (a, h + v.toInt, d + a * v.toInt)
          case s"down $v"    => (a + v.toInt, h, d)
          case s"up $v"      => (a - v.toInt, h, d)
        }
    }
    h * d
  }
}
