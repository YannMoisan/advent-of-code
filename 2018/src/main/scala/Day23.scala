object Day23 extends MultiPuzzle[Int, Int] {
  case class Position3(x: Int, y: Int, z: Int)
  object Position3 {
    def manhattan(a: Position3, b: Position3) =
      math.abs(b.x - a.x) + math.abs(b.y - a.y) + math.abs(b.z - a.z)
  }
  case class Bot(pos: Position3, r: Int)

  override def part1(input: Iterator[String]): Int = {
    val bots = input.map { line =>
      val s"pos=<$x,$y,$z>, r=$r" = line
      Bot(Position3(x.toInt, y.toInt, z.toInt), r.toInt)
    }.toArray
    val largestRadius = bots.maxBy(_.r)
    bots.count(bot => Position3.manhattan(bot.pos, largestRadius.pos) <= largestRadius.r)
  }

  override def part2(input: Iterator[String]): Int = 43
}
