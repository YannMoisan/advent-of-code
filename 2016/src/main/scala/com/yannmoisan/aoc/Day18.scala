package com.yannmoisan.aoc

object Day18 extends Puzzle {

  def isTrap(s: String) = s match {
    case "^^." => '^'
    case ".^^" => '^'
    case "..^" => '^'
    case "^.." => '^'
    case _ => '.'
  }

  def newLine(line: String) = s".$line.".sliding(3).map(isTrap).mkString

  def part(n: Int) = { lines: Seq[String] =>
    val grid = (1 until n).scanLeft(lines.head) { case (acc, _) => newLine(acc) }
    grid.map(_.count(_ == '.')).sum
  }

  override def part1 = part(40)

  override def part2 = part(400000)

}
