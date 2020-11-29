package com.yannmoisan.aoc

object Day6 extends Puzzle {
  override def part1: (Seq[String]) => String = part(_.head)

  override def part2: (Seq[String]) => String = part(_.last)

  def part(f: Seq[Char] => Char) = { lines: Seq[String] =>
    lines
      .transpose
      .map(orderedByFreq)
      .map(f)
      .mkString("")
  }

  def orderedByFreq[A](s: Seq[A]): Seq[A] = s.groupBy(identity).mapValues(_.size).toList.sortBy(-_._2).map(_._1)
}
