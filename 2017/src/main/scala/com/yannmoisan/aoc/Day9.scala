package com.yannmoisan.aoc

object Day9 extends SinglePuzzle[Int, Int] {
  val garbage = "<(?:!.|[^>])*>"

  override def part1 = { line =>
    val cleaned = line.replaceAll(garbage, "")

    final case class State(level: Int, sum: Int)
    val state0 = State(1, 0)

    cleaned
      .foldLeft(state0) {
        case (s, c) =>
          c match {
            case '{' => State(s.level + 1, s.sum + s.level)
            case '}' => State(s.level - 1, s.sum)
            case _   => s
          }
      }
      .sum
  }

  override def part2 = { line =>
    garbage.r
      .findAllIn(line)
      .map(_.replaceAll("!.", "").length - 2)
      .sum
  }
}
