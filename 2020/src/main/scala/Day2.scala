object Day2 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int =
    input.count {
      case s"$lo-$hi $ch: $pwd" =>
        val count = pwd.count(_ == ch.head)
        count <= hi.toInt && count >= lo.toInt
    }

  override def part2(input: Iterator[String]): Int =
    input.count {
      case s"$lo-$hi $ch: $pwd" =>
        pwd.charAt(lo.toInt - 1) == ch.head ^ pwd.charAt(hi.toInt - 1) == ch.head
    }

}
