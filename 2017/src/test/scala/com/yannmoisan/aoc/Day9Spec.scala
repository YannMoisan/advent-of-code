package com.yannmoisan.aoc
import com.yannmoisan.aoc.Puzzles._
import org.scalatest.{FlatSpec, Matchers}

class Day9Spec extends FlatSpec with Matchers {
  "Day9" should "answer part1" in {
    runPart1(Day9) should ===(14204)
  }

  it should "answer part2" in {
    runPart2(Day9) should ===(6622)
  }
}
