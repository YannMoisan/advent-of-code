import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day11.part1(Day11.input) shouldEqual 1661
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day11.part2(Day11.input) shouldEqual 334
  }
}
