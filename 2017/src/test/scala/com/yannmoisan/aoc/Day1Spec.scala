package com.yannmoisan.aoc
import com.yannmoisan.aoc.Puzzles._
import org.scalatest.{FlatSpec, Matchers}

class Day1Spec extends FlatSpec with Matchers {
  "Day1" should "answer part1" in {
    runPart1(Day1) should ===(1158)
  }

  it should "answer part2" in {
    runPart2(Day1) should ===(1132)
  }
}
