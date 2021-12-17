import scala.collection.mutable

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
object Day11 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val grid = input.map(_.toCharArray.map(_.toString.toInt)).toArray
    (1 to 100).map(_ => flash(grid)).sum
  }

  override def part2(input: Iterator[String]): Int = {
    val grid = input.map(_.toCharArray.map(_.toString.toInt)).toArray
    Iterator.from(1).find(_ => flash(grid) == 100).get

  }

  def neighbors(g: Array[Array[Int]], x: Int, y: Int): Seq[(Int, Int)] =
    for {
      dx <- -1 to 1
      dy <- -1 to 1
      xx = x + dx
      yy = y + dy
      if xx >= 0 && yy >= 0 && xx < g.head.length && yy < g.length && (dx != 0 || dy != 0)
    } yield (x + dx, y + dy)

  def flash(g: Array[Array[Int]]): Int = {
    val flash = mutable.Set[(Int, Int)]()

    for {
      x <- 0 until g.head.length
      y <- 0 until g.length
    } {
      g(y)(x) = g(y)(x) + 1
    }

    var candidates: Seq[(Int, Int)] = for {
      x <- 0 until g.head.length
      y <- 0 until g.length
      if g(y)(x) > 9 && !flash.contains((x, y))
    } yield (x, y)

    while (!candidates.isEmpty) {
      candidates.foreach {
        case (x, y) =>
          val _ = flash.add((x, y))
          neighbors(g, x, y).foreach { case (x, y) => g(y)(x) = g(y)(x) + 1 }
      }
      candidates = for {
        x <- 0 until g.head.length
        y <- 0 until g.length
        if g(y)(x) > 9 && !flash.contains((x, y))
      } yield (x, y)
    }

    for {
      x <- 0 until g.head.length
      y <- 0 until g.length
      if g(y)(x) > 9
    } {
      g(y)(x) = 0
    }
    flash.size
  }
}
