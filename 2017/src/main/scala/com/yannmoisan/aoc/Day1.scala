package com.yannmoisan.aoc

object Day1 extends SinglePuzzle[Int, Int] {
  def captcha(f: Int => Int): String => Int = { line: String =>
    (0 until line.length)
      .map(i =>
        if (line(i) == line(f(i) % line.length)) line(i).toString.toInt else 0)
      .sum
  }

  override def part1 = captcha(_ + 1)

  override def part2 = { line =>
    captcha(_ + line.length / 2)(line)
  }
}
