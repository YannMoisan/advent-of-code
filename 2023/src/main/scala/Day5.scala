object Day5 extends MultiPuzzle[Long, Long] {
  case class Interval(from: Long, until: Long, f: Long => Long) {
    def intersect(range: ConvertRange): List[Interval] = {
      val intersectFrom  = math.max(from, range.srcRangeStart)
      val intersectUntil = math.min(until, range.srcRangeStart + range.rangeLength)
      val res =
        if (intersectUntil > intersectFrom)
          List(
            if (from != intersectFrom) Some(Interval(from, intersectFrom, identity)) else None,
            Some(
              Interval(
                intersectFrom,
                intersectUntil,
                x => x + (range.destRangeStart - range.srcRangeStart)
              )
            ),
            if (intersectUntil != until) Some(Interval(intersectUntil, until, identity)) else None
          ).flatten
        else {
          List(this)
        }
      res
    }
  }

  def eureka(intervals0: List[Interval], ranges: List[ConvertRange]): List[Interval] = {
    var intervals = intervals0
    ranges.foreach(range => intervals = intervals.map(i => i.intersect(range)).reduce(_ ++ _))
    intervals
  }

  def translate(intervals: List[Interval]): List[Interval] =
    intervals.map(inter => Interval(inter.f(inter.from), inter.f(inter.until), identity))

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

  override def part2(input: Iterator[String]): Long = {
    val lines = input.toArray
    val seeds = parseSeeds(lines(0))
    var inters: List[Interval] =
      seeds
        .grouped(2).collect {
          case List(start, length) => Interval(start, start + length, identity)
        }.toList
    val converterA = parseConverter(lines, 4, 12)
    val converterB = parseConverter(lines, 15, 57)
    val converterC = parseConverter(lines, 60, 105)
    val converterD = parseConverter(lines, 108, 147)
    val converterE = parseConverter(lines, 150, 186)
    val converterF = parseConverter(lines, 189, 206)
    val converterG = parseConverter(lines, 209, 250)

    val converters =
      List(converterA, converterB, converterC, converterD, converterE, converterF, converterG)

    converters.foreach(converter => inters = translate(eureka(inters, converter.ranges)))

    inters.map(_.from).min
  }
}
