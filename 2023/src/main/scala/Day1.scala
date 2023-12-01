object Day1 extends MultiPuzzle[Int, Int] {
  private val m = Map(
    "one"   -> "1",
    "two"   -> "2",
    "three" -> "3",
    "four"  -> "4",
    "five"  -> "5",
    "six"   -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine"  -> "9"
  )

  override def part1(input: Iterator[String]): Int =
    common(input, m.values)

  override def part2(input: Iterator[String]): Int =
    common(input, m.keys ++ m.values)

  def common(input: Iterator[String], terms: Iterable[String]): Int = {
    def value(s: String): Int = {
      var first: Option[String] = None
      var last: Option[String]  = None

      var i = 0
      while (i < s.length) {
        terms
          .find(term => s.substring(i).startsWith(term))
          .foreach { term =>
            val k = m.getOrElse(term, term)
            if (first.isEmpty) {
              first = Some(k)
            }
            last = Some(k)
          }
        i += 1
      }
      s"${first.get}${last.get}".toInt
    }

    input.map(value).sum
  }
}
