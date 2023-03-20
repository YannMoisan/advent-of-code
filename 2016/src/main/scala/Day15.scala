object Day15 extends MultiPuzzle[Int, Int] {
  val line = """Disc #\d has (\d+) positions; at time=0, it is at position (\d+).""".r

  def parse: String => Disc = {
    case line(nbPos, pos) => Disc(nbPos.toInt, pos.toInt)
  }

  def ok(discs: Seq[Disc])(time: Int) =
    (1 to discs.size).forall(i => (time + i + discs(i - 1).pos) % discs(i - 1).nbPos == 0)

  case class Disc(nbPos: Int, pos: Int)

  override def part1(lines: Iterator[String]) = {
    val discs = lines.map(parse)
    LazyList.from(0).find(ok(discs.toList)).get
  }

  override def part2(lines: Iterator[String]) = {
    val discs = lines.toList.map(parse) :+ Disc(11, 0)
    LazyList.from(0).find(ok(discs)).get
  }
}
