object Day8 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int =
    input.map(line => line.length - (replace(line).length - 2)).sum

  def replace(s: String): String =
    s.replaceAll("""\\\\""", "z")
      .replaceAll("""\\"""", "z")
      .replaceAll("""\\x..""", "z")

  def replace2(s: String): String =
    s.replace("\\", """\\""")
      .replace("\"", """\"""")

  override def part2(input: Iterator[String]): Int =
    input.map(line => replace2(line).length + 2 - line.length).sum

}
