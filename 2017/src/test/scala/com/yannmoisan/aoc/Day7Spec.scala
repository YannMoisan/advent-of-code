package com.yannmoisan.aoc
import com.yannmoisan.aoc.Puzzles._
import org.scalatest.{FlatSpec, Matchers}

class Day7Spec extends FlatSpec with Matchers {
  "Day7" should "answer part1" in {
    runPart1(Day7) should ===("qibuqqg")
  }
}
