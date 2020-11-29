package com.yannmoisan.aoc
import com.yannmoisan.aoc.Puzzles._
import org.scalatest.{FlatSpec, Matchers}

class Day2Spec extends FlatSpec with Matchers {
  "Day2" should "answer part1" in {
    runPart1(Day2) should ===(30994)
  }

  it should "answer part2" in {
    runPart2(Day2) should ===(233)
  }
}
