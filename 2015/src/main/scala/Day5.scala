object Day5 extends MultiPuzzle[Int, Int] {
  def countVowels(s: String): Boolean =
    s.count(c => Set('a', 'e', 'i', 'o', 'u').contains(c)) >= 3

  override def part1(input: Iterator[String]): Int =
    input.count(s => countVowels(s) && twiceInARow(s) && doNotContains(s))

  override def part2(input: Iterator[String]): Int = 42

  def twiceInARow(s: String): Boolean =
    s.toSeq.sliding(2).exists(s => s(0) == s(1))

  def doNotContains(s: String): Boolean =
    Seq("ab", "cd", "pq", "xy").forall(!s.contains(_))

}
