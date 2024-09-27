case class Rectangle(id: Int, x: Int, y: Int, w: Int, h: Int) {
  def corners = {
    val x2 = x + w - 1
    val y2 = y + h - 1
    Seq((x, y), (x2, y), (x, y2), (x2, y2))
  }
}

object Day3 extends MultiPuzzle[Int, Int] {
  override def part1(iter: Iterator[String]): Int = {
    val lines                  = iter.toArray
    val parsedLines            = lines.map(parseLine)
    val arr: Array[Array[Int]] = Array.fill(1000)(Array.fill(1000)(0))
    for (elem <- parsedLines)
      for {
        x <- (elem.x until elem.x + elem.w)
        y <- (elem.y until elem.y + elem.h)
      } arr(y)(x) = arr(y)(x) + 1
    arr.map(a => a.count(_ >= 2)).sum
  }

  def parseLine(s: String): Rectangle = {
    val Pattern = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r
    s match {
      case Pattern(a, b, c, d, e) =>
        Rectangle(a.toInt, b.toInt, c.toInt, d.toInt, e.toInt)
    }
  }

  // cf: https://stackoverflow.com/questions/13390333/two-rectangles-intersection
  def overlap(r1: Rectangle, r2: Rectangle): Boolean =
    !(r1.x + r1.w < r2.x || r2.x + r2.w < r1.x || r1.y + r1.h < r2.y || r2.y + r2.h < r1.y)
  //r1.corners.exists { case (x, y) => isInRect(r2, x, y)}

  def isInRect(r: Rectangle, x: Int, y: Int): Boolean =
    x >= r.x && x < r.x + r.w && y >= r.y && y < r.y + r.h

  override def part2(iter: Iterator[String]): Int = {
    val lines       = iter.toArray
    val parsedLines = lines.map(parseLine)
    val ids = (for {
      r1 <- parsedLines
      r2 <- parsedLines
      if r1.id < r2.id
      if overlap(r1, r2) || overlap(r2, r1)
    } yield Seq(r1.id, r2.id)).flatten.toSet
    val remainingIds = (1 to 1287).filterNot(i => ids(i))
    println(remainingIds.mkString(","))
    remainingIds.head
  }
}
