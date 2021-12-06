import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day6Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day6.part1(Day6.input) shouldEqual 350149
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day6.part2(Day6.input) shouldEqual 1590327954513L
  }
}
