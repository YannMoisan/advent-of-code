object Day2 extends MultiPuzzle[Int, Int] {
  case class Game(id: Int, drawings: List[Draw])
  case class Draw(red: Int, green: Int, blue: Int)

  def parseDraw(s: String): Draw = {
    val m = s
      .split(", ").map { s =>
        val s"$nr $color" = s
        (color, nr.toInt)
      }.toMap
    Draw(m.getOrElse("red", 0), m.getOrElse("green", 0), m.getOrElse("blue", 0))
  }

  def parseGame(s: String): Array[Draw] =
    s.split("; ").map(parseDraw)

  def parseLine(s: String): Game = {
    val s"Game $id: $game" = s
    Game(id.toInt, parseGame(game).toList)
  }

  override def part1(input: Iterator[String]): Int =
    input
      .map(parseLine)
      .collect {
        case g if g.drawings.forall(m => m.red <= 12 && m.green <= 13 && m.blue <= 14) => g.id
      }
      .sum

  override def part2(input: Iterator[String]): Int =
    input.map { s =>
      val game     = parseLine(s)
      val maxRed   = game.drawings.map(_.red).max
      val maxGreen = game.drawings.map(_.green).max
      val maxBlue  = game.drawings.map(_.blue).max
      maxRed * maxGreen * maxBlue
    }.sum
}
