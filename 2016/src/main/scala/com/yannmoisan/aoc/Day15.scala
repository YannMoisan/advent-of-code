package com.yannmoisan.aoc

object Day15 extends Puzzle {
  val line = """Disc #\d has (\d+) positions; at time=0, it is at position (\d+).""".r

  def parse: String => Disc = {
    case line(nbPos, pos) => Disc(nbPos.toInt, pos.toInt)
  }

  def ok(discs: Seq[Disc])(time: Int) = (1 to discs.size).forall(i => (time + i + discs(i - 1).pos) % discs(i - 1).nbPos == 0)

  case class Disc(nbPos: Int, pos: Int)

  override def part1 = { lines =>
    val discs = lines.map(parse)
    Stream.from(0).find(ok(discs)).get
  }

  override def part2 = { lines =>
    val discs = lines.map(parse) :+ Disc(11, 0)
    Stream.from(0).find(ok(discs)).get
  }
}
