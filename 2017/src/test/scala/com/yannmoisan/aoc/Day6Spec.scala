package com.yannmoisan.aoc
import com.yannmoisan.aoc.Puzzles._
import org.scalatest.{FlatSpec, Matchers}

class Day6Spec extends FlatSpec with Matchers {
  "Day6" should "answer part1" in {
    runPart1(Day6) should ===(5042)
  }

  it should "answer part2" in {
    runPart2(Day6) should ===(1086)
  }
}
