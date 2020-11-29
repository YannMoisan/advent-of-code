package com.yannmoisan.aoc
import com.yannmoisan.aoc.Puzzles._
import org.scalatest.{FlatSpec, Matchers}

class Day5Spec extends FlatSpec with Matchers {
  "Day5" should "answer part1" in {
    runPart1(Day5) should ===(372671)
  }

  it should "answer part2" in {
    runPart2(Day5) should ===(25608480)
  }
}
