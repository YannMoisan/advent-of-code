package com.yannmoisan.aoc
import com.yannmoisan.aoc.Puzzles._
import org.scalatest.{FlatSpec, Matchers}

class Day3Spec extends FlatSpec with Matchers {
  "Day3" should "answer part1" in {
    runPart1(Day3) should ===(371)
  }
}
