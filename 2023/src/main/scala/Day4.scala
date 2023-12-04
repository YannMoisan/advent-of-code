import scala.collection.mutable

object Day4 extends MultiPuzzle[Int, Int] {
  case class Card(id: Int, winningNumbers: Set[Int], myNumbers: Set[Int]) {
    val matchCount: Int =
      myNumbers.intersect(winningNumbers).size

    val score: Int =
      if (matchCount == 0) 0 else math.pow(2, matchCount.toDouble - 1).toInt
  }
  def parse(s: String): Card = {
    val s"Card $id: $winningNumbers | $myNumbers" = s
    Card(
      id.trim.toInt,
      winningNumbers.split(" +").filter(_ != "").map(_.toInt).toSet,
      myNumbers.split(" +").filter(_ != "").map(_.toInt).toSet
    )
  }

  override def part1(input: Iterator[String]): Int =
    input.map(parse).map(_.score).sum

  override def part2(input: Iterator[String]): Int = {
    val cards = input.map(parse).toArray

    val counts = mutable.Map.from((1 to cards.length).map(i => i -> 1))
    var sum    = 0
    (1 to cards.length).foreach { i =>
      sum += counts(i)
      val score = cards(i - 1).matchCount
      (i + 1 to i + score).foreach(j => if (j <= cards.length) counts(j) += counts(i))
    }
    sum
  }
}
