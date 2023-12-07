import scala.collection.immutable.Map

object Day7 extends MultiPuzzle[Int, Int] {
  val cardValue1: Map[Char, Int] =
    Map('A' -> 14, 'K' -> 13, 'Q' -> 12, 'J' -> 11, 'T' -> 10) ++
      ('1' to '9').map(c => (c, c.asDigit))

  val cardValue2: Map[Char, Int] = cardValue1 + ('J' -> 1)

  def distinctAndMaxPart1(hand: String): (Int, Int) = {
    val grouped = hand.groupBy(identity)
    val max     = grouped.values.toList.map(_.length).max
    (grouped.size, max)
  }

  def distinctAndMaxPart2(hand: String): (Int, Int) = {
    val jCount  = hand.count(_ == 'J')
    val grouped = hand.filter(_ != 'J').groupBy(identity)
    val max     = if (grouped.isEmpty) 0 else grouped.values.toList.map(_.length).max
    (grouped.size, max + jCount)
  }

  def valueHand(hand: String, distinctAndMax: String => (Int, Int), cardValue: Map[Char, Int]) = {
    val typ = distinctAndMax(hand) match {
      case (0, _) => 10
      case (1, _) => 10
      case (2, 4) => 9
      case (2, 3) => 8
      case (3, 3) => 7
      case (3, 2) => 6
      case (4, _) => 5
      case (5, _) => 4
    }
    15 * 15 * 15 * 15 * 15 * typ +
      15 * 15 * 15 * 15 * cardValue(hand(0)) +
      15 * 15 * 15 * cardValue(hand(1)) +
      15 * 15 * cardValue(hand(2)) +
      15 * cardValue(hand(3)) +
      cardValue(hand(4))
  }

  override def part1(input: Iterator[String]): Int =
    common(input, valueHand(_, distinctAndMaxPart1, cardValue1))

  override def part2(input: Iterator[String]): Int =
    common(input, valueHand(_, distinctAndMaxPart2, cardValue2))

  def common(input: Iterator[String], value: String => Int): Int = {
    val hands = input.map { s =>
      val Array(hand, bid) = s.split(' ')
      (hand, bid.toInt)
    }.toList
    hands
      .sortBy { case (hand, _) => value(hand) }
      .zipWithIndex
      .map { case ((_, bid), index) => bid * (index + 1) }
      .sum
  }
}
