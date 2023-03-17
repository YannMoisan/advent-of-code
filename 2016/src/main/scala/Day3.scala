object Day3 extends MultiPuzzle[Int, Int] {

  def parse: Iterator[String] => Seq[Array[Int]] =
    _.map(_.split("\\s+").tail.map(_.toInt)).toList

  def countTriangles(s: Seq[Seq[Int]]) =
    s.map(_.sortBy(identity))
      .filter(a => a(0) + a(1) > a(2))
      .size

  override def part1(lines: Iterator[String]): Int = {
    val triangles = parse(lines).map(_.toList)
    countTriangles(triangles)
  }

  override def part2(lines: Iterator[String]): Int = {
    val triangles = parse(lines).transpose.flatten.grouped(3).toList
    countTriangles(triangles)
  }
}
