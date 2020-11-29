package com.yannmoisan.aoc
import com.yannmoisan.aoc.Puzzles._
import org.scalatest.{FlatSpec, Matchers}

class Day8Spec extends FlatSpec with Matchers {
  "Day8" should "answer part1" in {
    runPart1(Day8) should ===(5946)
  }

  it should "answer part2" in {
    runPart2(Day8) should ===(6026)
  }
}
