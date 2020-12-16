import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day15Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day15.part1(Day15.input) shouldEqual 959
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day15.part2(Day15.input) shouldEqual 116590
  }
}
