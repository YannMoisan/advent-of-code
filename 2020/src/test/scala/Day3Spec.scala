import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day3Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day3.part1(Day3.input) shouldEqual 272L
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day3.part2(Day3.input) shouldEqual 3898725600L
  }
}
