import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day14Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day14.part1(Day14.input) shouldEqual 7477696999511L
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day14.part2(Day14.input) shouldEqual 3687727854171L
  }
}
