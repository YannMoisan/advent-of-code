import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day3Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day3.part1(Day3.input) shouldEqual 2035764
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day3.part2(Day3.input) shouldEqual 2817661
  }
}
