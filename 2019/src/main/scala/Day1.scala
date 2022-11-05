import scala.annotation.tailrec

object Day1 extends MultiPuzzle[Int, Int] {

  override def part1(lines: Iterator[String]) : Int = {
    lines.map(_.toInt).map(fuel).sum
  }

  override def part2(lines: Iterator[String]) : Int = {
    lines.map(_.toInt).map(recFuel(_, 0)).sum
  }

  private def fuel(i: Int): Int = i / 3 - 2

  @tailrec
  def recFuel(i: Int, acc: Int): Int = {
    val f = fuel(i)
    if (f >= 0) recFuel(f, acc + f) else acc
  }
}
