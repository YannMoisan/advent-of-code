package com.yannmoisan.aoc
import com.yannmoisan.aoc.Puzzles._
import org.scalatest.{FlatSpec, Matchers}

class Day4Spec extends FlatSpec with Matchers {
  "Day4" should "answer part1" in {
    runPart1(Day4) should ===(455)
  }

  it should "answer part2" in {
    runPart2(Day4) should ===(186)
  }
}
