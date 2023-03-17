import com.yannmoisan.util.fp.loop

object Day3 extends MultiPuzzle[Int, Int] {
  case class State(numbers: Array[String], pos: Int)

  override def part1(input: Iterator[String]): Int = {
    val lines   = input.toArray
    val counts  = count1Bits(lines)
    val gamma   = mostCommonBit(counts, lines.length, '1').mkString
    val epsilon = leastCommonBit(counts, lines.length, '1').mkString
    parseBinaryString(gamma) * parseBinaryString(epsilon)
  }

  override def part2(input: Iterator[String]): Int = {
    val lines = input.toArray

    val oxygenRating = loop(State(lines, 0))(
      s => {
        val counts = count1Bits(s.numbers)
        val c      = mostCommonBit(counts, s.numbers.length, '1')(s.pos)
        State(s.numbers.filter(l => l(s.pos) == c), s.pos + 1)
      },
      _.numbers.length == 1
    ).numbers.head

    val co2Rating = loop(State(lines, 0))(
      s => {
        val counts = count1Bits(s.numbers)
        val c      = leastCommonBit(counts, s.numbers.length, '0')(s.pos)
        State(s.numbers.filter(l => l(s.pos) == c), s.pos + 1)
      },
      _.numbers.length == 1
    ).numbers.head

    parseBinaryString(oxygenRating) * parseBinaryString(co2Rating)
  }

  private def count1Bits(lines: Array[String]): Vector[Int] = {
    val len = lines.head.length
    lines.foldLeft(Vector.fill(len)(0)) {
      case (s, line) =>
        Vector.tabulate(len)(i => s(i) + (if (line(i) == '1') 1 else 0))
    }
  }

  private def mostCommonBit(vec: Vector[Int], length: Int, resolveTie: Char): Vector[Char] =
    vec.map { c =>
      if (c > length - c) '1'
      else if (c < length - c) '0'
      else resolveTie
    }

  private def leastCommonBit(vec: Vector[Int], length: Int, resolveTie: Char): Vector[Char] =
    vec.map { c =>
      if (c > length - c) '0'
      else if (c < length - c) '1'
      else resolveTie
    }

  private def parseBinaryString(s: String): Int = Integer.parseInt(s, 2)
}
