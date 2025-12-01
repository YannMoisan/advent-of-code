object Day1 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val positions = input.scanLeft(50) {
      case (pos, line) =>
        val rotation = parseLine(line)
        Math.floorMod(pos + rotation, 100)
    }
    positions.count(_ == 0)
  }

  private def parseLine: String => Int = {
    case s"L$n" => -n.toInt
    case s"R$n" => n.toInt
  }

  override def part2(input: Iterator[String]): Int = {
    val result = input.foldLeft((50, 0)) {
      case ((pos, zeros), line) =>
        val rotation = parseLine(line)
        val newPos   = pos + rotation
        val zerosInc = {
          if (newPos >= 100) (newPos / 100)
          else if (newPos <= 0) (-newPos / 100) + (if (pos == 0) 0 else 1)
          else 0
        }
        (Math.floorMod(newPos, 100), zeros + zerosInc)
    }
    result._2
  }
}
