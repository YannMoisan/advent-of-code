object Day5 extends SinglePuzzle[Int, Int] {
  override def part1(s: String): Int =
    react(s)

  def react(s: String): Int = {
    var i = 0
    var t = s
    while (i < t.length - 1) {
      if (t(i).toUpper == t(i + 1).toUpper && (t(i).isUpper ^ t(i + 1).isUpper)) {
        t = t.substring(0, i) + t.substring(i + 2, t.length)
        i = math.max(0, i - 1)
      } else {
        i += 1
      }
    }
    t.length
  }

  override def part2(s: String): Int =
    ('a' to 'z')
      .map(c => s.replaceAll(s"($c|${c.toUpper})", ""))
      .map(react)
      .min
}
