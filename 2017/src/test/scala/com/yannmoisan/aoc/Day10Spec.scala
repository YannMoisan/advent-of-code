package com.yannmoisan.aoc
import com.yannmoisan.aoc.Puzzles._
import org.scalatest.{FlatSpec, Matchers}

class Day100Spec extends FlatSpec with Matchers {
  "Day10" should "answer part1" in {
    runPart1(Day10) should ===(1980)
  }

  it should "answer part2" in {
    runPart2(Day10) should ===("899124dac21012ebc32e2f4d11eaec55")
  }
}
