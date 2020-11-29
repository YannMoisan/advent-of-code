package com.yannmoisan.aoc

object Day20 extends Puzzle {

  case class Range(start: Long, end: Long)

  def parse(line: String): Range = {
    val Array(a, b) = line.split("-")
    Range(a.toLong, b.toLong)
  }

  def mergeRange(ranges: Seq[Range]): Seq[Range] = {
    val sorted = ranges.sortBy(_.start)
    sorted.foldLeft(List[Range]()) { case (acc, range) =>
      acc match {
        case h :: t if h.end + 1 >= range.start => Range(h.start, math.max(h.end, range.end)) :: t
        case _ => range :: acc
      }
    }.reverse
  }

  override def part1 = { lines =>
    val parsed = lines.map(parse)
    val merged = mergeRange(parsed)
    merged.head.end + 1
  }

  override def part2 = { lines =>
    val parsed = lines.map(parse)
    val merged = mergeRange(parsed)
    merged.sliding(2).map(t => t(1).start - 1 - t(0).end).sum
  }
}
