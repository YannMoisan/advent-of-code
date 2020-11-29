package com.yannmoisan.aoc

object Day3 extends SinglePuzzle[Int, Unit] {
  override def part1 = { line =>
    def closest(i: Int, values: Seq[Int]): Int = {
      values.map(v => math.abs(v - i)).min
    }

    def values(circle: Int): Seq[Int] = {
      val step = circle * 2
      val first = math.pow(circle * 2 - 1, 2).toInt + (circle)

      (0 to 3).map(i => first + (i * +step))
    }

    def circle(i: Int): Int = {
      val sqrt = math.sqrt(i - 1).toInt
      val tmp = if (sqrt % 2 == 0) sqrt else sqrt + 1
      tmp / 2
    }

    val i = line.toInt
    circle(i) + closest(i, values(circle(i)))
  }

  override def part2 = { line =>
    }
}
