// label:regex
object Day5 extends MultiPuzzle[Int, Int] {
  def countVowels(s: String): Boolean =
    s.count(c => Set('a', 'e', 'i', 'o', 'u').contains(c)) >= 3

  override def part1(input: Iterator[String]): Int =
    input.count(s => countVowels(s) && twiceInARow(s) && doNotContains(s))

  override def part2(input: Iterator[String]): Int =
    input.count(s => repeatWithOneLetterBetween(s) && repeatingPair(s))

  def twiceInARow(s: String): Boolean =
    s.toSeq.sliding(2).exists(s => s(0) == s(1))

  def repeatWithOneLetterBetween(s: String): Boolean =
    s.toSeq.sliding(3).exists(s => s(0) == s(2))

  // abcde
  def repeatingPair(s: String): Boolean = {
    val tmp = for {
      start  <- 0 to s.length - 4
      offset <- start + 2 to s.length - 2
    } yield s.substring(start, start + 2) == s.substring(offset, offset + 2)
    tmp.exists(identity)
  }

  // 411 too high

  def doNotContains(s: String): Boolean =
    Seq("ab", "cd", "pq", "xy").forall(!s.contains(_))

}
