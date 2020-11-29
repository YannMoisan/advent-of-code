import com.yannmoisan.aoc.Day2
import com.yannmoisan.aoc.Puzzles._

import org.scalatest._

class Day2Spec extends FlatSpec with Matchers {
  "Day2" should "answer part1" in {
    part1(Day2) should === ("12578")
  }

  it should "answer part2" in {
    part2(Day2) should === ("516DD")
  }
}
