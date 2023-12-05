object Day5 extends MultiPuzzle[Long, Int] {
  case class ConvertRange(destRangeStart: Long, srcRangeStart: Long, rangeLength: Int) {
    def apply: PartialFunction[Long, Long] = {
      case i: Long if i >= srcRangeStart && i < srcRangeStart + rangeLength =>
        destRangeStart + (i - srcRangeStart)
    }
  }
  case class Converter(ranges: List[ConvertRange]) {
    def apply(i: Long): Long =
      ranges.map(_.apply).reduce(_ orElse _).applyOrElse(i, (x: Long) => x)
  }

  def parseSeeds(s: String): List[Long] = {
    val s"seeds: $seeds" = s
    seeds.split(' ').map(_.toLong).toList
  }

  def parseRange(s: String): ConvertRange = {
    val s"$a $b $c" = s
    ConvertRange(a.toLong, b.toLong, c.toInt)
  }
  def parseConverter(lines: Array[String], start: Int, end: Int): Converter =
    Converter(lines.slice(start - 1, end).map(parseRange).toList)

  override def part1(input: Iterator[String]): Long = {
    val lines      = input.toArray
    val seeds      = parseSeeds(lines(0))
    val converterA = parseConverter(lines, 4, 12)
    val converterB = parseConverter(lines, 15, 57)
    val converterC = parseConverter(lines, 60, 105)
    val converterD = parseConverter(lines, 108, 147)
    val converterE = parseConverter(lines, 150, 186)
    val converterF = parseConverter(lines, 189, 206)
    val converterG = parseConverter(lines, 209, 250)

    val converters =
      List(converterA, converterB, converterC, converterD, converterE, converterF, converterG)

    seeds.map(seed => converters.foldLeft(seed) { case (i, conv) => conv.apply(i) }).min
  }

  override def part2(input: Iterator[String]): Int = 42
}
