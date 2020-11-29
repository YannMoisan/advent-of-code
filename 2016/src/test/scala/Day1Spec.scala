import com.yannmoisan.aoc.Day1
import com.yannmoisan.aoc.Puzzles._

import org.scalatest._

class Day1Spec extends FlatSpec with Matchers {
  "Day1" should "answer part1" in {
    part1(Day1) should === (300)
  }

  it should "answer part2" in {
    part2(Day1) should === (159)
  }
}
