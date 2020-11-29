package com.yannmoisan.aoc

object Day4 extends MultiPuzzle[Int, Int] {
  def valid(f: String => String): Iterator[String] => Int = {
    lines: Iterator[String] =>
      lines.map { line =>
        if (line.split(" ").groupBy(f).exists(_._2.size > 1)) 0 else 1
      }.sum
  }

  override def part1 = valid(identity)

  override def part2 = valid(_.sorted)
}
