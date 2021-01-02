import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day24Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on input" in {
    Day24.part1(Day24.input) shouldEqual 346
  }

  behavior of "Part 2"

  it should "work on input" in {
    Day24.part2(Day24.input) shouldEqual 3802
  }
}
