object Day1 extends SinglePuzzle[Int, Int] {
  def captcha(f: Int => Int): String => Int = { line: String =>
    (0 until line.length)
      .map(i =>
        if (line(i) == line(f(i) % line.length)) line(i).toString.toInt else 0)
      .sum
  }

  override def part1(s: String) = captcha(_ + 1)(s)

  override def part2(line: String) = {
    captcha(_ + line.length / 2)(line)
  }
}
